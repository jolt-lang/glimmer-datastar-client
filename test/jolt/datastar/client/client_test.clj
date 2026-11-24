(ns jolt.datastar.client.client-test
  "The client against a scripted transport: what it sends, how it applies what
  comes back, and its retry and cancellation rules."
  (:require [clojure.test :refer [deftest is]]
            [jolt.datastar.client :as ds]
            [jolt.datastar.client.store :as store]))

(defn- fake-transport
  "A :subscribe that records each request and hands its handlers to the test,
  which plays the server by calling them."
  []
  (let [calls (atom [])]
    {:calls calls
     :subscribe (fn [req handlers]
                  (let [closed (atom false)]
                    (swap! calls conj {:req req :handlers handlers :closed closed})
                    {:close (fn [] (reset! closed true)
                              ((:on-close handlers) {:id nil :closed? true}))
                     :done (promise)}))}))

(defn- timers []
  (let [q (atom [])]
    {:queue q
     :timer (fn [ms thunk] (swap! q conj [ms thunk]) (fn []))
     :run-next! (fn [] (let [[_ t] (first @q)] (swap! q (comp vec rest)) (t)))}))

(defn- client [t ts & {:as opts}]
  (ds/client (merge {:url "http://example.test/app/"
                     :root [:vbox {:id "app"}]
                     :subscribe (:subscribe t)
                     :on-ui (fn [f] (f))
                     :timer (:timer ts)
                     :on-error (fn [_ _])}
                    opts)))

(defn- event! [call event data] ((get-in call [:handlers :on-event]) {:event event :data data}))
(defn- close! [call m] ((get-in call [:handlers :on-close]) m))

(deftest urls-resolve-against-the-client-url
  (is (= "http://h/x" (ds/resolve-url "http://h/app/" "/x")))
  (is (= "http://h/app/x" (ds/resolve-url "http://h/app/page" "x")))
  (is (= "https://o/y" (ds/resolve-url "http://h/app/" "https://o/y"))))

(deftest connect-sends-signals-and-applies-the-stream
  (let [t (fake-transport) ts (timers)
        c (client t ts :signals {:q "hi" :_local 1})]
    (ds/connect! c)
    (let [{:keys [req] :as call} (first @(:calls t))]
      (is (= :get (:method req)))
      (is (= "http://example.test/app/" (:url req)))
      (is (= {"datastar" "{:q \"hi\"}"} (:query-params req)) "signals, less the local ones")
      (is (= "true" (get-in req [:headers "datastar-request"])))
      (is (= "text/event-stream, application/edn" (get-in req [:headers "Accept"])))
      (event! call "datastar-patch-elements" "elements [:vbox {:id \"app\"} [:label {:id \"n\" :data-text (str $count)}]]")
      (event! call "datastar-patch-signals" "signals {:count 3}")
      (is (= 3 (:count (ds/signals c))))
      (is (= [:vbox {:id "app"} [:jolt.datastar.client.store/ref "n" 0]]
             (store/element (:store c) "app"))))))

(deftest actions-post-their-signals-as-the-body
  (let [t (fake-transport) ts (timers) c (client t ts :signals {:n 1})]
    (store/patch! (:store c) {:mode :outer :elements [[:vbox {:id "app"}
                                                       [:button {:id "inc" :data-on {:click '(POST "/inc")}}]]]})
    ((:fetch (:env (:store c))) {:method :post :url "/inc" :signals {:n 1} :opts {} :el "inc"})
    (let [{:keys [req]} (first @(:calls t))]
      (is (= "http://example.test/inc" (:url req)))
      (is (= "{:n 1}" (:body req)))
      (is (= "application/edn" (get-in req [:headers "Content-Type"]))))))

(deftest a-plain-reply-of-the-codecs-type-patches-by-id
  (let [t (fake-transport) ts (timers) c (client t ts)]
    (ds/connect! c)
    ((get-in (first @(:calls t)) [:handlers :on-response])
     {:status 200 :headers {"Content-Type" "application/edn"}
      :body (java.io.ByteArrayInputStream. (.getBytes "[:vbox {:id \"app\"} [:label \"plain\"]]"))})
    (is (= [:vbox {:id "app"} [:label {} "plain"]] (store/element (:store c) "app")))))

(deftest network-errors-retry-with-backoff-then-give-up
  (let [t (fake-transport) ts (timers) errors (atom 0) done (atom false)
        c (client t ts :retry-max-count 2 :on-error (fn [_ _] (swap! errors inc)))]
    (ds/fetch! c {:method :get :url "/" :signals {} :opts {} :el "x" :done #(reset! done true)})
    (close! (first @(:calls t)) {:error (ex-info "reset" {})})
    (is (= [1000.0] (map (comp double first) @(:queue ts))) "first retry after the interval")
    ((:run-next! ts))
    (close! (second @(:calls t)) {:error (ex-info "reset" {})})
    (is (= 2000.0 (double (ffirst @(:queue ts)))) "doubled")
    ((:run-next! ts))
    (close! (nth @(:calls t) 2) {:error (ex-info "reset" {})})
    (is (= 3 (count @(:calls t))))
    (is (empty? @(:queue ts)) "gave up after :retry-max-count")
    (is (= 1 @errors))
    (is @done)))

(deftest the-servers-retry-and-last-id-are-used
  (let [t (fake-transport) ts (timers) c (client t ts)]
    (ds/fetch! c {:method :get :url "/" :signals {} :opts {} :el "x"})
    (let [call (first @(:calls t))]
      ((get-in call [:handlers :on-retry]) 250)
      (close! call {:id "17" :error (ex-info "reset" {})}))
    (is (= 250.0 (double (ffirst @(:queue ts)))))
    ((:run-next! ts))
    (is (= "17" (get-in (second @(:calls t)) [:handlers :last-event-id])))))

(deftest a-clean-end-is-not-retried-unless-always
  (let [t (fake-transport) ts (timers) c (client t ts)]
    (ds/fetch! c {:method :get :url "/" :signals {} :opts {} :el "x"})
    (close! (first @(:calls t)) {:id nil})
    (is (empty? @(:queue ts))))
  (let [t (fake-transport) ts (timers) c (client t ts :retry :always)]
    (ds/fetch! c {:method :get :url "/" :signals {} :opts {} :el "x"})
    (close! (first @(:calls t)) {:id nil})
    (is (= 1 (count @(:queue ts))))))

(deftest a-new-request-from-an-element-cancels-its-last
  (let [t (fake-transport) ts (timers) c (client t ts)]
    (ds/fetch! c {:method :get :url "/a" :signals {} :opts {} :el "search"})
    (ds/fetch! c {:method :get :url "/b" :signals {} :opts {} :el "search"})
    (ds/fetch! c {:method :get :url "/c" :signals {} :opts {} :el "other"})
    (is (= [true false false] (map (comp deref :closed) @(:calls t))))
    (is (empty? @(:queue ts)) "a cancelled request is not retried")))

(deftest a-plain-map-reply-patches-signals
  (let [t (fake-transport) ts (timers) c (client t ts)]
    (ds/connect! c)
    ((get-in (first @(:calls t)) [:handlers :on-response])
     {:status 200 :headers {"Content-Type" "application/edn"}
      :body (java.io.ByteArrayInputStream. (.getBytes "{:saved true}"))})
    (is (true? (:saved (ds/signals c))))))
