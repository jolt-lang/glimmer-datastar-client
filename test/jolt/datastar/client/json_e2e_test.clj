(ns jolt.datastar.client.json-e2e-test
  "examples/json_counter over real HTTP: glimmer-datastar's JSON middleware on
  the server, the JSON codec on the client, the page drawn through glimmer."
  (:require [clojure.test :refer [deftest is]]
            [json-counter.server :as server]
            [jolt.datastar.client :as ds]
            [jolt.datastar.client.codec :as codec]
            [jolt.datastar.client.test-backend :as tb]
            [ring-chez.adapter :as adapter]))

(defn- wait-for [pred]
  (loop [i 0] (cond (pred) true (> i 100) false :else (do (Thread/sleep 50) (recur (inc i))))))

(defn- click! [root label]
  ((:on-click (:props @(tb/find-by root #(= label (:label %)))))))

(deftest the-json-counter-round-trip
  (reset! server/count* 0)
  (let [srv (adapter/run-server server/handler {:port 0})
        c (ds/client {:url (str "http://127.0.0.1:" (:port srv) "/")
                      :codec codec/json
                      :root [:vbox {:id "app"}]
                      :on-ui (fn [f] (f))})
        root (tb/mount (ds/view c))
        count-label #(some-> (tb/by-id root "count") deref :props :label)]
    (try
      (ds/connect! c)
      (is (wait-for #(= "count: 0" (count-label))) "the JSON page arrived")
      (is (= 1 (:step (ds/signals c))) "with its data-signals")
      (click! root "step 5")                  ; a local action: no request
      (is (= 5 (:step (ds/signals c))))
      (is (wait-for #(= "step: 5" (:label (:props @(tb/find-by root (fn [p] (some-> (:label p) (.startsWith "step:")))))))))
      (click! root "add")                     ; POST /add with the signals as JSON
      (is (wait-for #(= "count: 5" (count-label))) "the server read $step from the JSON body")
      (is (wait-for #(= 5 (:total (ds/signals c)))))
      (is (wait-for #(false? (:saving (ds/signals c)))) "the indicator cleared")
      (finally (adapter/stop-server srv)))))
