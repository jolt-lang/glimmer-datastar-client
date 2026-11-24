(ns jolt.datastar.client.attrs-test
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer.backend :as b]
            [glimmer.core :as ui]
            [jolt.datastar.client.attrs :as attrs]
            [jolt.datastar.client.signals :as sig]))

(defn- env
  ([] (env {}))
  ([signals] (let [s (sig/store)] (sig/merge-patch! s signals) {:signals s})))

(defn- render [tag props e] (attrs/render-props tag props e (atom {})))

(defn- fake-timer []
  (let [now (atom 0) queue (atom [])]
    {:now (fn [] @now)
     :timer (fn [ms thunk]
              (let [job {:at (+ @now ms) :thunk thunk :live (atom true)}]
                (swap! queue conj job)
                (fn [] (reset! (:live job) false))))
     :advance! (fn [ms]
                 (let [target (+ @now ms)]
                   (loop []
                     (let [due (sort-by :at (filter #(<= (:at %) target) @queue))]
                       (if-let [j (first due)]
                         (do (swap! queue #(vec (remove #{j} %)))
                             (reset! now (:at j))
                             (when @(:live j) ((:thunk j)))
                             (recur))
                         (reset! now target))))))}))

;; --- per render ----------------------------------------------------------------

(deftest text-show-and-attr
  (let [e (env {:count 3 :open true :busy false})]
    (is (= {:id "c" :label "Count: 3"}
           (render :label {:id "c" :data-text '(str "Count: " $count)} e)))
    (is (= {:text "3"} (render :entry {:data-text '(str $count)} e)) "an entry's text is :text")
    (is (= {:label "x" :sensitive true}
           (render :button {:label "x" :data-attr {:sensitive '(not $busy)}} e)))
    (is (some? (render :label {:data-show '$open} e)))
    (is (nil? (render :label {:data-show '(not $open)} e)))))

(deftest data-on-runs-actions-with-the-event-value
  (let [e (env {:count 0 :q ""})
        btn (render :button {:data-on {:click '(swap! $count inc)}} e)
        ent (render :entry {:data-on {:change '(set! $q evt)}} e)]
    ((:on-click btn))
    ((:on-click btn))
    ((:on-change ent) "hello")
    (is (= {:count 2 :q "hello"} (sig/snapshot (:signals e))))))

(deftest a-failing-handler-is-reported-not-thrown
  (let [errors (atom [])
        e (assoc (env) :on-error (fn [_ form] (swap! errors conj form)))
        btn (render :button {:data-on {:click '(swap! $n inc)}} e)]
    ((:on-click btn))                       ; inc of nil throws
    (is (= ['(swap! $n inc)] @errors))))

(deftest data-bind-is-two-way
  (let [e (env {:name "ada" :agree false})
        ent (render :entry {:data-bind :name} e)
        chk (render :checkbutton {:data-bind :agree :label "ok"} e)]
    (is (= "ada" (:text ent)))
    (is (= false (:active chk)))
    ((:on-change ent) "bob")
    ((:on-toggled chk))
    (is (= {:name "bob" :agree true} (sig/snapshot (:signals e))))
    (testing "bind and data-on on the same event both run"
      (let [log (render :entry {:data-bind :name :data-on {:change '(set! $echo evt)}} e)]
        ((:on-change log) "zed")
        (is (= ["zed" "zed"] [(sig/value (:signals e) :name) (sig/value (:signals e) :echo)]))))))

(deftest modifiers-debounce-throttle-once
  (let [{:keys [advance!] :as t} (fake-timer)
        e (merge (env {:n 0 :m 0 :o 0}) (select-keys t [:timer :now]))
        state (atom {})
        props {:data-on {:change {:do '(swap! $n inc) :debounce 100}
                         :click {:do '(swap! $m inc) :throttle 100}
                         :activate {:do '(swap! $o inc) :once true}}}
        h (attrs/render-props :entry props e state)
        v #(sig/value (:signals e) %)]
    (dotimes [_ 5] ((:on-change h) "x") (advance! 20))
    (is (= 0 (v :n)) "still typing")
    (advance! 100)
    (is (= 1 (v :n)) "one run after the pause")
    ((:on-click h)) ((:on-click h)) (advance! 150) ((:on-click h))
    (is (= 2 (v :m)) "leading edge, then again after the window")
    (let [h2 (attrs/render-props :entry props e state)]   ; a re-render keeps the state
      ((:on-activate h)) ((:on-activate h2)))
    (is (= 1 (v :o)))))

(deftest indicator-is-true-while-a-request-runs
  (let [pending (atom nil)
        e (assoc (env) :fetch (fn [req] (reset! pending (:done req))))
        btn (render :button {:data-indicator :saving :data-on {:click '(POST "/save")}} e)]
    ((:on-click btn))
    (is (true? (sig/value (:signals e) :saving)))
    (@pending)
    (is (false? (sig/value (:signals e) :saving)))))

;; --- per element lifetime --------------------------------------------------------

(deftest mount-merges-signals-and-runs-init
  (let [e (env {:a 1})
        stop (attrs/mount! :vbox {:data-signals {:b 2}
                                  :data-signals-if-missing {:a 99 :c 3}
                                  :data-init '(set! $started true)} e)]
    (is (= {:a 1 :b 2 :c 3 :started true} (sig/snapshot (:signals e))))
    (stop)))

(deftest a-bound-signal-starts-from-the-elements-value
  (let [e (env)]
    (attrs/mount! :entry {:text "initial" :data-bind :q} e)
    (is (= "initial" (sig/value (:signals e) :q)))))

(deftest computed-follows-its-sources-until-stopped
  (let [e (env {:price 10 :qty 2})
        stop (attrs/mount! :label {:data-computed {:total '(* $price $qty)}} e)
        total #(sig/value (:signals e) :total)]
    (is (= 20 (total)))
    (sig/set-value! (:signals e) :qty 3)
    (is (= 30 (total)))
    (stop)
    (sig/set-value! (:signals e) :qty 4)
    (is (= 30 (total)) "stopped")))

(deftest effect-reruns-on-change
  (let [e (env {:q "a"})
        stop (attrs/mount! :label {:data-effect '(set! $echo (str $q "!"))} e)]
    (sig/set-value! (:signals e) :q "b")
    (is (= "b!" (sig/value (:signals e) :echo)))
    (stop)))

(deftest interval-ticks-until-stopped
  (let [{:keys [advance!] :as t} (fake-timer)
        e (merge (env {:ticks 0}) (select-keys t [:timer :now]))
        stop (attrs/mount! :label {:data-on-interval {:do '(swap! $ticks inc) :ms 100}} e)]
    (advance! 350)
    (is (= 3 (sig/value (:signals e) :ticks)))
    (stop)
    (advance! 500)
    (is (= 3 (sig/value (:signals e) :ticks)))))

(deftest signal-patch-listener-filters-by-name
  (let [e (env)
        stop (attrs/mount! :label {:data-on-signal-patch {:do '(swap! $seen inc) :include "^user\\."}} e)]
    (sig/set-value! (:signals e) :seen 0)
    (sig/merge-patch! (:signals e) {:user {:name "x"}})
    (sig/merge-patch! (:signals e) {:other 1})
    (is (= 1 (sig/value (:signals e) :seen)))
    (stop)))

;; --- through glimmer: a signal re-renders only the element that reads it ------

(defn- tree-backend []
  (let [ids (atom 0)]
    {:name :tree
     :create! (fn [tag props] (atom {:id (swap! ids inc) :tag tag :props props :children []}))
     :apply-props! (fn [_ w props] (swap! w assoc :props props))
     :append-child! (fn [_ p c] (swap! p update :children conj c))
     :remove-child! (fn [_ p c] (swap! p update :children (fn [cs] (vec (remove #(identical? % c) cs)))))
     :replace-child! (fn [_ p o n] (swap! p update :children (fn [cs] (mapv #(if (identical? % o) n %) cs))))}))

(deftest a-signal-change-re-renders-only-its-reader
  (b/register! (tree-backend))
  (let [e (env {:a 1 :b 1})
        renders (atom {})
        el (fn [tag props]
             (swap! renders update (:id props) (fnil inc 0))
             (when-let [p (attrs/render-props tag props e (atom {}))] [tag p]))
        root (atom {:id 0 :tag :root :props {} :children []})]
    (ui/mount root :root [:vbox [el :label {:id "a" :data-text '(str $a)}]
                                [el :label {:id "b" :data-text '(str $b)}]])
    (sig/set-value! (:signals e) :a 2)
    (let [labels (mapv #(:label (:props @%)) (:children @(first (:children @root))))]
      (is (= ["2" "1"] labels)))
    (is (= {"a" 2 "b" 1} @renders))))
