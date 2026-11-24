(ns jolt.datastar.client.protocol-test
  (:require [clojure.test :refer [deftest is testing]]
            [jolt.datastar.client.codec :as codec]
            [jolt.datastar.client.protocol :as protocol]))

(defn- read-ev [event data] (protocol/read-event codec/edn {:event event :data data}))

(deftest patch-elements-events
  (is (= {:type :patch-elements :selector "#list" :mode :inner
          :elements [[:vbox {:id "list"} [:label {:data-text '(str $n)}]] [:label "two"]]
          :use-view-transition false}
         (read-ev "datastar-patch-elements"
                  "selector #list\nmode inner\nelements [:vbox {:id \"list\"}\nelements  [:label {:data-text (str $n)}]]\nelements [:label \"two\"]")))
  (testing "outer by id is the default"
    (is (= {:type :patch-elements :selector nil :mode :outer :elements [[:p {:id "x"}]]
            :use-view-transition true}
           (read-ev "datastar-patch-elements" "useViewTransition true\nelements [:p {:id \"x\"}]")))))

(deftest malformed-patches-are-refused
  (is (thrown-with-msg? Exception #"unknown patch mode" (read-ev "datastar-patch-elements" "mode sideways")))
  (is (thrown-with-msg? Exception #"only #id selectors" (read-ev "datastar-patch-elements" "selector .row\nmode inner")))
  (is (thrown-with-msg? Exception #"needs a selector" (read-ev "datastar-patch-elements" "mode append"))))

(deftest patch-signals-events
  (is (= {:type :patch-signals :signals {:a 1 :b {:c nil}} :only-if-missing true}
         (read-ev "datastar-patch-signals" "onlyIfMissing true\nsignals {:a 1\nsignals  :b {:c nil}}"))))

(deftest other-events-are-not-datastars
  (is (nil? (read-ev "message" "hello"))))

(deftest the-edn-codec
  (testing "encodes JSON-compatible signals, and refuses what JSON can't carry"
    (is (= {:a [1 "x" true nil] :b {:c 2.5}}
           ((:decode codec/edn) ((:encode codec/edn) {:a [1 "x" true nil] :b {:c 2.5}}))))
    (is (thrown-with-msg? Exception #"signal mode: :fast has no JSON form"
                          ((:encode codec/edn) {:mode :fast})))
    (is (thrown-with-msg? Exception #"signal tags: #\{1\} has no JSON form"
                          ((:encode codec/edn) {:tags #{1}}))))
  (testing "reads no tags but EDN's own (#inst, #uuid)"
    (is (thrown-with-msg? Exception #"unknown tag" ((:decode codec/edn) "#my/thing {:a 1}")))))

(deftest the-json-codec
  (is (= "{\"form\":{\"name\":\"ada\"},\"n\":1}" ((:encode codec/json) {:form {:name "ada"} :n 1})))
  (is (= {:a {:b [1 nil]}} ((:decode codec/json) "{\"a\": {\"b\": [1, null]}}")))
  (testing "elements are JSON hiccup, one or many"
    (is (= [[:vbox {:id "app" :data-on {:click "(swap! $n inc)"}} [:label {:label "x"}] "text"]]
           ((:decode-elements codec/json)
            "[\"vbox\", {\"id\": \"app\", \"data-on\": {\"click\": \"(swap! $n inc)\"}}, [\"label\", {\"label\": \"x\"}], \"text\"]")))
    (is (= [[:p {:id "a"}] [:p {:id "b"}]]
           ((:decode-elements codec/json) "[[\"p\", {\"id\": \"a\"}], [\"p\", {\"id\": \"b\"}]]"))))
  (testing "a patch-elements event read with it"
    (is (= [[:label {:id "n" :label "3"}]]
           (:elements (protocol/read-event codec/json {:event "datastar-patch-elements"
                                                      :data "elements [\"label\", {\"id\": \"n\", \"label\": \"3\"}]"}))))))
