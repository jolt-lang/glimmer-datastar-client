(ns jolt.datastar.client
  "A Datastar thin client: the page is server-sent data, drawn by whichever
  glimmer backend the app requires, and every interaction is a request whose
  response patches the page.

      (require '[glimmer.core :as ui]
               '[glimmer-gtk.core]                 ; or any glimmer backend
               '[jolt.datastar.client :as ds])

      (def c (ds/client {:url \"http://localhost:3000/\"
                         :root [:vbox {:id \"app\"}]}))
      (ds/connect! c)                              ; GET :url, applying what it streams
      (ui/run (fn [] (ds/view c)) :title \"app\")

  Options:
    :url        the server; relative action URLs resolve against it
    :root       the element drawn until the server patches it (needs an :id)
    :codec      how data goes on the wire (jolt.datastar.client.codec), EDN by
                default
    :signals    initial signals
    :retry      :auto (retry after a network error or a 5xx), :always (also
                after a stream ends), :never
    :retry-interval 1000, :retry-scaler 2, :retry-max-wait 30000,
    :retry-max-count 10     Datastar's defaults
    :headers    sent with every request
    :on-error   (fn [exception form-or-nil]) for failures; default prints
  Hooks for tests and other hosts:
    :subscribe  (fn [request handlers]) -> {:close :done}, jolt.http.sse's
    :on-ui      (fn [thunk]) runs on the UI thread, glimmer.core/on-gui's
    :timer      (fn [ms thunk]) -> cancel fn; :now (fn []) -> ms"
  (:require [clojure.string :as str]
            [glimmer.core :as ui]
            [jolt.datastar.client.codec :as codec]
            [jolt.datastar.client.protocol :as protocol]
            [jolt.datastar.client.signals :as sig]
            [jolt.datastar.client.store :as store]))

;; --- defaults ---------------------------------------------------------------------

(defn- default-subscribe [request handlers]
  ;; resolved when first used, so a host with its own :subscribe never loads
  ;; the HTTP stack
  ((requiring-resolve 'jolt.http.sse/subscribe!) request handlers))

(defn- default-timer [on-ui]
  (fn [ms thunk]
    (let [live (atom true)]
      (future (Thread/sleep (long ms))
              (when @live (on-ui #(when @live (thunk)))))
      (fn [] (reset! live false) nil))))

(defn- report [on-error]
  (or on-error
      (fn [e _] (binding [*out* *err*] (println "datastar client:" (ex-message e))))))

;; --- urls -------------------------------------------------------------------------

(defn resolve-url
  "`url` against `base`: absolute URLs stand, /paths take base's origin,
  others are relative to base's directory."
  [base url]
  (cond
    (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*://" url) url
    (nil? base) url
    :else (let [[_ origin path] (re-find #"^([a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]*)([^?#]*)" base)]
            (if (str/starts-with? url "/")
              (str origin url)
              (str origin (subs path 0 (inc (or (str/last-index-of path "/") -1))) url)))))

;; --- applying ---------------------------------------------------------------------

(defn apply-patch!
  "Apply one decoded patch (jolt.datastar.client.protocol/read-event) to the
  page. Runs on the calling thread; `handle-event` puts it on the UI thread."
  [c patch]
  (case (:type patch)
    :patch-elements (store/patch! (:store c) patch)
    :patch-signals (sig/merge-patch! (:signals c) (:signals patch)
                                     {:only-if-missing (:only-if-missing patch)})
    nil))

(defn- handle-event [c e]
  (try
    (when-let [patch (protocol/read-event (:codec c) e)]
      ((:on-ui c) #(try (apply-patch! c patch)
                        (catch Exception ex ((:on-error c) ex nil)))))
    (catch Exception ex ((:on-error c) ex nil))))

(defn- content-type [resp]
  (some (fn [[k v]] (when (= "content-type" (str/lower-case (name k))) (str v))) (:headers resp)))

(defn- handle-response
  "A reply that is not an event stream, of the codec's type. As in Datastar,
  where a JSON object reply patches signals and an HTML reply patches elements:
  a body that decodes to a map is a signals patch, and anything else is
  elements, patched by id."
  [c resp]
  (let [body (some-> (:body resp) slurp)
        ct (some-> (content-type resp) (str/split #";") first str/trim str/lower-case)]
    (cond
      (not (<= 200 (:status resp) 299))
      ((:on-error c) (ex-info (str "datastar client: " (:status resp) " from server")
                              {:status (:status resp) :body body}) nil)
      (and (= ct (:content-type (:codec c))) (not (str/blank? body)))
      (let [v ((:decode (:codec c)) body)
            patch (if (map? v)
                    {:type :patch-signals :signals v}
                    {:type :patch-elements :mode :outer :elements ((:decode-elements (:codec c)) body)})]
        ((:on-ui c) #(apply-patch! c patch)))
      :else nil)))

;; --- requests ---------------------------------------------------------------------

(defn- http-request
  "The jolt.http-client request for an action."
  [c {:keys [method url signals opts]}]
  (let [{:keys [content-type encode]} (:codec c)
        encoded (encode signals)
        headers (merge {"datastar-request" "true"
                        "Accept" (str "text/event-stream, " content-type)}
                       (:headers c) (:headers opts))]
    (merge {:method method :url (resolve-url (:url c) url) :headers headers}
           (if (#{:get :delete} method)
             {:query-params {"datastar" encoded}}
             {:body encoded :headers (assoc headers "Content-Type" content-type)}))))

(defn- backoff [c attempt server-retry]
  (min (:retry-max-wait c)
       (* (or server-retry (:retry-interval c)) (Math/pow (:retry-scaler c) attempt))))

(defn fetch!
  "Perform action `req` ({:method :url :signals :opts :el :done}, what
  expression actions produce): send it, apply what comes back, retry by the
  client's policy, and call :done when it is over for good. A new request from
  the same element cancels the one before it (Datastar's requestCancellation
  auto). Returns nil."
  [c {:keys [el done] :as req}]
  (let [request (http-request c req)
        policy (get-in req [:opts :retry] (:retry c))
        cancelled (atom false)
        current (atom nil)
        server-retry (atom nil)
        me {:cancelled cancelled :current current}
        finish (fn [] (when el (swap! (:inflight c) (fn [m] (if (identical? (get m el) me) (dissoc m el) m))))
                 (when done ((:on-ui c) done)))
        attempt (fn attempt [n last-id]
                  (reset! current
                          ((:subscribe c) request
                           {:last-event-id last-id
                            :on-open (fn [_] (reset! server-retry nil))
                            :on-event #(handle-event c %)
                            :on-retry #(reset! server-retry %)
                            :on-response #(handle-response c %)
                            :on-close
                            (fn [{:keys [id error response closed?]}]
                              (let [failed? (or error (some-> response :status (>= 500)))
                                    again? (and (not closed?) (not @cancelled)
                                                (< n (:retry-max-count c))
                                                (case policy
                                                  :never false
                                                  :always (or failed? (nil? response))
                                                  failed?))]
                                (when (and error (not again?)) ((:on-error c) error nil))
                                (if again?
                                  ((:timer c) (backoff c n @server-retry)
                                   #(when-not @cancelled (attempt (inc n) id)))
                                  (finish))))})))]
    (when el
      (when-let [{prev :cancelled prev-sub :current} (get @(:inflight c) el)]
        (reset! prev true)
        (when-let [close (some-> prev-sub deref :close)] (close)))
      (swap! (:inflight c) assoc el me))
    (attempt 0 nil)
    nil))

;; --- public -----------------------------------------------------------------------

(defn client
  "A client for the server at (:url opts); see the namespace doc for options."
  [opts]
  (let [on-ui (or (:on-ui opts) ui/on-gui)
        signals (sig/store)
        c (merge {:retry :auto :retry-interval 1000 :retry-scaler 2
                  :retry-max-wait 30000 :retry-max-count 10
                  :codec codec/edn :subscribe default-subscribe}
                  (select-keys opts [:url :codec :retry :retry-interval :retry-scaler :retry-max-wait
                                    :retry-max-count :headers :subscribe])
                  {:on-ui on-ui
                   :timer (or (:timer opts) (default-timer on-ui))
                   :now (or (:now opts) #(System/currentTimeMillis))
                   :on-error (report (:on-error opts))
                   :signals signals
                   :inflight (atom {})})
        c-ref (volatile! nil)
        env {:signals signals
             :fetch (fn [req] (fetch! @c-ref req))
             :timer (:timer c) :now (:now c) :on-error (:on-error c)}
        c (assoc c :store (store/store env))]
    (vreset! c-ref c)
    (when-let [s (:signals opts)] (sig/merge-patch! signals s))
    (when-let [root (:root opts)] (store/mount-root! (:store c) root))
    c))

(defn connect!
  "GET the client's :url (or `path` against it) with the signals, as a page's
  data-init does, applying what the server streams back."
  ([c] (connect! c (:url c)))
  ([c path]
   (fetch! c {:method :get :url path :signals (sig/snapshot (:signals c)) :opts {} :el ::page})))

(defn view
  "The page as glimmer hiccup: render it from the app's root component."
  [c]
  (store/view (:store c)))

(defn signals
  "The client's signals, as data (for tools and tests)."
  [c]
  (sig/snapshot (:signals c) {:include-local true}))
