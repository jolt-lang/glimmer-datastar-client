(ns jolt.datastar.client.expr-test
  (:require [clojure.test :refer [deftest is]]
            [jolt.datastar.client.expr :as expr]
            [jolt.datastar.client.signals :as sig]))

(defn- env
  ([] (env {}))
  ([signals] (let [s (sig/store)] (sig/merge-patch! s signals) {:signals s})))

(defn- ev [form e] (expr/evaluate form e))
(defn- act [form e] (expr/evaluate form (assoc e :action? true)))

(deftest reads-signals-and-computes
  (let [e (env {:count 2 :user {:name "ada"}})]
    (is (= 3 (ev '(inc $count) e)))
    (is (= "Hi ada" (ev '(str "Hi " $user.name) e)))
    (is (= [2 "ada"] (ev '[$count $user.name] e)))
    (is (= {:n 2} (ev '{:n $count} e)))
    (is (nil? (ev '$missing e)))))

(deftest special-forms
  (let [e (env {:n 5})]
    (is (= :big (ev '(if (> $n 3) :big :small) e)))
    (is (= :mid (ev '(cond (> $n 9) :huge (> $n 3) :mid :else :small) e)))
    (is (= "five" (ev '(case $n 4 "four" 5 "five" "other") e)))
    (is (= 12 (ev '(let [x (* $n 2) y (+ x 2)] y) e)))
    (is (= false (ev '(and true (< $n 3) (/ 1 0)) e)) "and short-circuits")
    (is (= 5 (ev '(or nil $n) e)))
    (is (= "x" (ev '(when (pos? $n) "x") e)))
    (is (= "got abc" (ev '(str "got " evt) (assoc e :evt "abc"))))
    (is (= "row-1" (ev 'el (assoc e :el "row-1"))))))

(deftest unknown-names-fail-at-compile-time
  (is (thrown-with-msg? Exception #"unknown form" (expr/compile-expr '(slurp "/etc/passwd"))))
  (is (thrown-with-msg? Exception #"unknown symbol" (expr/compile-expr 'System)))
  (is (thrown-with-msg? Exception #"unknown function" (expr/compile-expr '(swap! $a spit)))))

(deftest actions-change-signals
  (let [e (env {:count 1 :open false :items {:a false :b true}})]
    (act '(swap! $count + 10) e)
    (act '(toggle! $open) e)
    (act '(set! $label (str "n=" $count)) e)
    (is (= {:count 11 :open true :label "n=11" :items {:a false :b true}} (sig/snapshot (:signals e))))
    (act '(set-all! "^items\\." true) e)
    (is (= {:a true :b true} (ev '$items e)))
    (act '(toggle-all! "^items\\.") e)
    (is (= {:a false :b false} (ev '$items e)))))

(deftest actions-are-refused-outside-action-contexts
  (let [e (env {:count 1})]
    (is (thrown-with-msg? Exception #"only allowed in an action" (ev '(set! $count 2) e)))
    (is (= 1 (ev '$count e)))))

(deftest server-actions-send-the-filtered-signals
  (let [sent (atom [])
        e (assoc (env {:name "ada" :_draft "x" :tab 2}) :fetch #(swap! sent conj %) :el "save")]
    (act '(POST "/save") e)
    (act '(GET (str "/tab/" $tab) {:include "^tab$" :headers {"x" "y"}}) e)
    (is (= [{:method :post :url "/save" :el "save" :signals {:name "ada" :tab 2} :opts {}}
            {:method :get :url "/tab/2" :el "save" :signals {:tab 2} :opts {:headers {"x" "y"}}}]
           @sent))))

(deftest peek-reads-without-subscribing
  (let [e (env {:a 1})]
    (is (= 1 (ev '(peek $a) e)))))

(deftest action?-finds-state-changes
  (is (expr/action? '(when $x (set! $y 1))))
  (is (not (expr/action? '(str $x)))))

;; --- animation, on a clock the test turns by hand ---------------------------

(defn- fake-clock []
  (let [now (atom 0) queue (atom [])]
    {:now (fn [] @now)
     :timer (fn [ms thunk]
              (let [job {:at (+ @now ms) :thunk thunk :live (atom true)}]
                (swap! queue conj job)
                (fn [] (reset! (:live job) false))))
     :advance! (fn [ms]
                 (swap! now + ms)
                 (loop []
                   (let [due (filter #(<= (:at %) @now) @queue)]
                     (when (seq due)
                       (swap! queue #(vec (remove (set due) %)))
                       (doseq [j due] (when @(:live j) ((:thunk j))))
                       (recur)))))}))

(deftest animate-tweens-a-signal-to-its-target
  (let [{:keys [advance!] :as clock} (fake-clock)
        e (merge (env {:x 0}) (select-keys clock [:now :timer]))]
    (act '(animate! $x {:to 100 :ms 160 :ease :linear}) e)
    (advance! 80)
    (is (< 40 (ev '$x e) 60) "half way, half done")
    (advance! 100)
    (is (= 100 (ev '$x e)))))

(deftest a-new-animation-replaces-a-running-one
  (let [{:keys [advance!] :as clock} (fake-clock)
        e (merge (env {:x 0}) (select-keys clock [:now :timer]))]
    (act '(animate! $x {:to 100 :ms 160 :ease :linear}) e)
    (advance! 80)
    (act '(animate! $x {:to 0 :ms 160 :ease :linear}) e)
    (advance! 400)
    (is (= 0 (ev '$x e)))))

(deftest string-expressions-are-source
  (let [e (env {:count 2})]
    (is (= 3 (ev "(inc $count)" e)))
    (is (= "n=2" (ev "(str \"n=\" $count)" e)) "inside a form a string is a string")
    (act "(swap! $count + 10)" e)
    (is (= 12 (ev "$count" e)))
    (is (thrown-with-msg? Exception #"unknown form" (expr/compile-expr "(slurp \"x\")")))
    (is (thrown-with-msg? Exception #"no tags allowed" (expr/compile-expr "#my/tag 1")))))
