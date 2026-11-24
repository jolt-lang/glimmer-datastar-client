(ns jolt.datastar.client.e2e-test
  "The whole loop over real HTTP: a ring-chez server streaming Datastar events,
  the client connecting through jolt.http.sse, an action posting signals back,
  and the page drawn through glimmer."
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [jolt.datastar.client :as ds]
            [jolt.datastar.client.test-backend :as tb]
            [ring-chez.adapter :as adapter]
            [ring-chez.sse :as sse]))

(def ^:private count* (atom 0))

(defn- page [n]
  (pr-str [:vbox {:id "app"}
           [:label {:id "count" :label (str "count: " n)}]
           [:button {:id "inc" :label "+1" :data-on {:click '(POST "/inc")}}]]))

(defn- events [& evs]
  (let [ch (async/chan 10)]
    (async/go (doseq [e evs] (async/>! ch e)) (async/close! ch))
    (sse/event-response ch)))

(defn- handler [{:keys [uri request-method body]}]
  (case [request-method uri]
    [:get "/"] (events {:event "datastar-patch-elements" :data (str "elements " (page @count*))})
    [:post "/inc"] (let [signals (edn/read-string (slurp body))
                         n (swap! count* + (:step signals 1))]
                     (events {:event "datastar-patch-elements"
                              :data (str "elements " (pr-str [:label {:id "count" :label (str "count: " n)}]))}
                             {:event "datastar-patch-signals" :data (str "signals " (pr-str {:last n}))}))
    {:status 404 :body "nope"}))

(defn- wait-for [pred]
  (loop [i 0] (cond (pred) true (> i 100) false :else (do (Thread/sleep 50) (recur (inc i))))))

(deftest a-page-streams-in-and-an-action-patches-it
  (let [server (adapter/run-server handler {:port 0})
        url (str "http://127.0.0.1:" (:port server) "/")
        c (ds/client {:url url :root [:vbox {:id "app"}] :signals {:step 2}
                      :on-ui (fn [f] (f))})
        root (tb/mount (ds/view c))
        label #(some-> (tb/by-id root "count") deref :props :label)]
    (try
      (ds/connect! c)
      (is (wait-for #(= "count: 0" (label))) "the page arrived")
      ((:on-click (:props @(tb/by-id root "inc"))))
      (is (wait-for #(= "count: 2" (label))) "the action's patch arrived")
      (is (= 2 (:last (ds/signals c))))
      (finally (adapter/stop-server server)))))
