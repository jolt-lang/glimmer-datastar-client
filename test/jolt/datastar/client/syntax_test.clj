(ns jolt.datastar.client.syntax-test
  (:require [clojure.test :refer [deftest is testing]]
            [jolt.datastar.client.signals :as sig]
            [jolt.datastar.client.store :as store]
            [jolt.datastar.client.syntax :as syntax]
            [jolt.datastar.client.test-backend :as tb]))

(deftest names-split-like-datastars
  (is (= {:plugin "data-on" :key "click" :mods {"debounce" #{"300ms" "leading"} "once" #{}}}
         (syntax/parse-name "data-on:click__debounce.300ms.leading__once")))
  (is (= {:plugin "data-on-interval" :key nil :mods {"duration" #{"2s"}}}
         (syntax/parse-name :data-on-interval__duration.2s)))
  (is (= "data-attr" (:plugin (syntax/parse-name "data-attr:aria-label"))))
  (is (nil? (syntax/parse-name :label))))

(deftest durations-and-casing
  (is (= [300 2000 150 7] [(syntax/ms #{"300ms"} 0) (syntax/ms #{"2s"} 0) (syntax/ms #{"150"} 0)
                           (syntax/ms #{"leading"} 7)]))
  (is (= "firstName" (syntax/cased "first-name" {})))
  (is (= "first-name" (syntax/cased "first-name" {"case" #{"kebab"}})))
  (is (= "first_name" (syntax/cased "first-name" {"case" #{"snake"}})))
  (is (= "FirstName" (syntax/cased "first-name" {"case" #{"pascal"}}))))

(deftest attributes-read-into-the-map-form
  (is (= {:id "x"
          :data-on {:click {:do "(POST \"/save\")" :debounce 300}
                    :change {:do "(set! $q evt)" :once true}}
          :data-signals-exprs {"firstName" "\"ada\""}
          :data-signals-if-missing-exprs {"count" "0"}
          :data-computed {"fullName" "(str $firstName \"!\")"}
          :data-attr {:sensitive "(not $busy)"}
          :data-bind "query"
          :data-indicator "saving"
          :data-on-interval {:do "(swap! $t inc)" :ms 500}
          :data-text "$firstName"
          :data-preserve-attr [:text :placeholder]}
         (syntax/canonical
          {:id "x"
           (keyword "data-on:click__debounce.300ms") "(POST \"/save\")"
           (keyword "data-on:change__once") "(set! $q evt)"
           (keyword "data-signals:first-name") "\"ada\""
           (keyword "data-signals:count__ifmissing") "0"
           (keyword "data-computed:full-name") "(str $firstName \"!\")"
           (keyword "data-attr:sensitive") "(not $busy)"
           (keyword "data-bind:query") ""
           (keyword "data-indicator:saving") ""
           (keyword "data-on-interval__duration.500ms") "(swap! $t inc)"
           :data-text "$firstName"
           :data-preserve-attr "text placeholder"})))
  (testing "merges with props already in the map form"
    (is (= {:data-on {:click {:do "a"} :change {:do "b"}}}
           (syntax/canonical {:data-on {:click {:do "a"}} (keyword "data-on:change") "b"}))))
  (testing "canonical props pass through"
    (let [p {:data-on-interval {:do "x" :ms 5} :data-signals {:a 1} :data-bind "q"}]
      (is (= p (syntax/canonical p))))))

(deftest a-page-in-datastar-syntax-works-end-to-end
  (let [s (sig/store)
        st (store/store {:signals s})
        root (do (store/mount-root! st [:vbox {:id "app"
                                                (keyword "data-signals:first-name") "\"ada\""
                                                (keyword "data-signals:count__ifmissing") "0"}
                                         [:label {:id "greet" :data-text "(str \"hi \" $firstName)"}]
                                         [:label {:id "full" (keyword "data-computed:shout") "(upper-case $firstName)"
                                                  :data-text "$shout"}]
                                         [:entry {:id "name" (keyword "data-bind:first-name") ""}]
                                         [:button {:id "inc" :label "+"
                                                   (keyword "data-on:click") "(swap! $count inc)"
                                                   (keyword "data-attr:sensitive") "(< $count 2)"}]])
                 (tb/mount (store/view st)))
        prop (fn [id k] (get-in @(tb/by-id root id) [:props k]))]
    (is (= "hi ada" (prop "greet" :label)))
    (is (= "ADA" (prop "full" :label)))
    (is (= "ada" (prop "name" :text)) "bound by Datastar's camelCased name")
    ((prop "name" :on-change) "bob")
    (is (= ["hi bob" "BOB"] [(prop "greet" :label) (prop "full" :label)]))
    ((prop "inc" :on-click))
    ((prop "inc" :on-click))
    (is (= 2 (sig/value s :count)))
    (is (false? (prop "inc" :sensitive)))))
