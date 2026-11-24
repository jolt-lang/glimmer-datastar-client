(ns jolt.datastar.client.store-test
  "The element store's patch modes, drawn through glimmer: what the page looks
  like after each, and which widgets survived it."
  (:require [clojure.test :refer [deftest is testing]]
            [jolt.datastar.client.signals :as sig]
            [jolt.datastar.client.store :as store]
            [jolt.datastar.client.test-backend :as tb]))

(defn- page
  "A store drawing `root`, mounted; returns [store root-widget signals]."
  [root]
  (let [s (sig/store)
        st (store/store {:signals s})]
    (store/mount-root! st root)
    [st (tb/mount (store/view st)) s]))

(defn- labels [w] (mapv #(:label (:props @%)) (:children @w)))
(defn- app [root] (first (:children @root)))

(def ^:private list-page
  [:vbox {:id "app"}
   [:label {:id "title" :label "Todos"}]
   [:vbox {:id "list"}
    [:label {:id "a" :label "a"}]
    [:label {:id "b" :label "b"}]]])

(defn- list-widget [root] (tb/by-id root "list"))

(deftest outer-by-id-morphs-in-place
  (let [[st root] (page list-page)
        a-widget (tb/by-id root "a")]
    (store/patch! st {:mode :outer :elements [[:label {:id "a" :label "A!"}]]})
    (is (= ["A!" "b"] (labels (list-widget root))))
    (is (identical? a-widget (tb/by-id root "a")) "the same widget, patched")))

(deftest outer-of-a-container-keeps-the-children-it-still-has
  (let [[st root] (page list-page)
        b-widget (tb/by-id root "b")]
    (store/patch! st {:mode :outer :elements [[:vbox {:id "list"}
                                                [:label {:id "c" :label "c"}]
                                                [:label {:id "b" :label "b2"}]]]})
    (is (= ["c" "b2"] (labels (list-widget root))))
    (is (identical? b-widget (tb/by-id root "b")) "b kept its widget across the reorder")
    (is (not (contains? (store/ids st) "a")) "a's cell is gone")))

(deftest inner-prepend-append
  (let [[st root] (page list-page)]
    (store/patch! st {:mode :append :selector "#list" :elements [[:label {:id "z" :label "z"}]]})
    (store/patch! st {:mode :prepend :selector "#list" :elements [[:label {:label "first"}]]})
    (is (= ["first" "a" "b" "z"] (labels (list-widget root))))
    (store/patch! st {:mode :inner :selector "#list" :elements [[:label {:label "only"}]]})
    (is (= ["only"] (labels (list-widget root))))
    (is (empty? (filter #{"a" "b" "z"} (store/ids st))))))

(deftest before-after-remove
  (let [[st root] (page list-page)]
    (store/patch! st {:mode :before :selector "#b" :elements [[:label {:id "x" :label "x"}]]})
    (store/patch! st {:mode :after :selector "#b" :elements [[:label {:id "y" :label "y"}]]})
    (is (= ["a" "x" "b" "y"] (labels (list-widget root))))
    (store/patch! st {:mode :remove :selector "#x" :elements []})
    (store/patch! st {:mode :remove :elements [[:label {:id "y"}]]})
    (is (= ["a" "b"] (labels (list-widget root))))))

(deftest replace-makes-new-widgets
  (let [[st root] (page list-page)
        a-widget (tb/by-id root "a")]
    (store/patch! st {:mode :replace :elements [[:label {:id "a" :label "fresh"}]]})
    (is (= ["fresh" "b"] (labels (list-widget root))))
    (is (not (identical? a-widget (tb/by-id root "a"))))))

(deftest replacing-the-root
  (let [[st root] (page list-page)]
    (store/patch! st {:mode :outer :elements [[:vbox {:id "app"} [:label {:label "new page"}]]]})
    (is (= ["new page"] (labels (app root))))
    (is (= #{"app"} (store/ids st)))))

(deftest a-patch-re-renders-only-its-element
  (let [[st root] (page list-page)
        before (mapv #(:id @%) (:children @(list-widget root)))
        title (tb/by-id root "title")
        title-props (:props @title)]
    (store/patch! st {:mode :outer :elements [[:label {:id "b" :label "b2"}]]})
    (is (= before (mapv #(:id @%) (:children @(list-widget root)))))
    (is (identical? title-props (:props @title)) "the title was not touched")))

(deftest unkeyed-siblings-match-by-tag-and-order
  (let [[st root] (page [:vbox {:id "app"} [:entry {:text "x"}] [:button {:label "go"}]])
        entry (tb/find-by root #(= "x" (:text %)))]
    (store/patch! st {:mode :outer :elements [[:vbox {:id "app"}
                                                [:label {:label "new"}]
                                                [:entry {:text "x"}]
                                                [:button {:label "go"}]]]})
    (is (identical? entry (tb/find-by root #(= "x" (:text %)))) "the entry kept its widget")))

(deftest ignore-morph-and-preserve-attr
  (let [[st root] (page [:vbox {:id "app"}
                         [:label {:id "keep" :label "mine" :data-ignore-morph true}]
                         [:entry {:id "e" :text "typed" :placeholder "p" :data-preserve-attr :text}]])]
    (store/patch! st {:mode :outer :elements [[:label {:id "keep" :label "theirs" :data-ignore-morph true}]
                                              [:entry {:id "e" :text "server" :placeholder "q"
                                                       :data-preserve-attr :text}]]})
    (is (= "mine" (:label (:props @(tb/by-id root "keep")))))
    (is (= ["typed" "q"] ((juxt :text :placeholder) (:props @(tb/by-id root "e")))))))

(deftest data-attributes-and-lifetimes-follow-the-page
  (let [[st root s] (page [:vbox {:id "app" :data-signals {:n 1}}
                           [:label {:id "n" :data-text '(str "n=" $n)}]
                           [:label {:id "derived" :data-computed {:double '(* 2 $n)}
                                    :data-text '(str $double)}]])]
    (is (= "n=1" (:label (:props @(tb/by-id root "n")))))
    (sig/set-value! s :n 5)
    (is (= ["n=5" "10"] (mapv #(:label (:props @(tb/by-id root %))) ["n" "derived"])))
    (testing "an element removed from the page stops its behaviours"
      (store/patch! st {:mode :remove :selector "#derived" :elements []})
      (sig/set-value! s :n 7)
      (is (= 10 (sig/value s :double))))))
