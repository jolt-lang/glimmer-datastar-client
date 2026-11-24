(ns jolt.datastar.client.signals-test
  (:require [clojure.test :refer [deftest is testing]]
            [glimmer.ratom :as r]
            [jolt.datastar.client.signals :as sig]))

(deftest paths-read-datastar-names
  (is (= [:form :name] (sig/path '$form.name)))
  (is (= [:form :name] (sig/path :form.name)))
  (is (= [:count] (sig/path "count")))
  (is (= "form.name" (sig/path-name [:form :name]))))

(deftest leaves-and-nested-reads
  (let [s (sig/store)]
    (sig/set-value! s :form {:name "ada" :age 36})
    (is (= "ada" (sig/value s :form.name)))
    (is (= {:name "ada" :age 36} (sig/value s :form)))
    (sig/set-value! s :form {:name "bob"})
    (is (= {:name "bob"} (sig/value s :form)) "a map replaces everything under it")
    (sig/set-value! s :form nil)
    (is (nil? (sig/value s :form)))))

(deftest merge-patch-follows-rfc-7386
  (let [s (sig/store)]
    (sig/merge-patch! s {:a 1 :b {:c 2 :d 3}})
    (sig/merge-patch! s {:a nil :b {:c 20}})
    (is (= {:b {:c 20 :d 3}} (sig/snapshot s)))
    (testing "only-if-missing writes absent signals only"
      (sig/merge-patch! s {:a 5 :b {:c 99}} {:only-if-missing true})
      (is (= {:a 5 :b {:c 20 :d 3}} (sig/snapshot s))))))

(deftest snapshot-leaves-local-signals-out
  (let [s (sig/store)]
    (sig/merge-patch! s {:name "x" :_open true :ui {:_hover 1 :tab 2}})
    (is (= {:name "x" :ui {:tab 2}} (sig/snapshot s)))
    (is (= {:name "x"} (sig/snapshot s {:include #"^name$"})))
    (is (= {:ui {:tab 2}} (sig/snapshot s {:exclude #"name"})))))

(deftest a-leaf-notifies-only-its-readers
  (let [s (sig/store) a-runs (atom 0) b-runs (atom 0)]
    (sig/merge-patch! s {:a 1 :b 1})
    (r/make-reaction #(do (sig/value s :a) (swap! a-runs inc)))
    (r/make-reaction #(do (sig/value s :b) (swap! b-runs inc)))
    (sig/set-value! s :a 2)
    (is (= [2 1] [@a-runs @b-runs]))))

(deftest a-signal-read-before-it-exists-is-told-when-it-arrives
  (let [s (sig/store)
        seen (r/make-reaction #(sig/value s :later))]
    (is (nil? @seen))
    (sig/merge-patch! s {:later "here"})
    (is (= "here" @seen))))

(deftest patch-listeners-hear-each-patch
  (let [s (sig/store) heard (atom [])
        stop (sig/on-patch! s #(swap! heard conj %))]
    (sig/merge-patch! s {:a 1})
    (stop)
    (sig/merge-patch! s {:a 2})
    (is (= [{:a 1}] @heard))
    (is (= [[[:a] 1] [[:b :c] nil]] (vec (sig/flatten-patch {:a 1 :b {:c nil}}))))))
