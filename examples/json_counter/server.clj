(ns json-counter.server
  "A Datastar server that speaks JSON, as Datastar servers do. The signals the
  client sends are parsed by glimmer-datastar's ordinary middleware
  (`wrap-signals`); the page goes out as JSON hiccup in the standard
  datastar-patch-elements events, where a browser app would send HTML. The
  logic (the count) lives here; the client only draws and sends input.

      jolt -M:examples -m json-counter.server        ; http://localhost:3000/"
  (:require [clojure.core.async :as async]
            [clojure.data.json :as json]
            [jolt.datastar.core :as datastar]
            [ring-chez.adapter :as adapter]
            [ring-chez.sse :as sse]))

(defonce count* (atom 0))

(defn- count-label [n]
  [:label {:id "count" :label (str "count: " n)}])

(defn page
  "The page, in Datastar's own attribute syntax (data-on:click, data-attr:...),
  as a Datastar server writes it. Expressions are strings, since JSON has no
  symbols or lists."
  [n]
  [:vbox {:id "app" :data-signals:step "1"}
   (count-label n)
   [:label {:data-text "(str \"step: \" $step)"}]
   [:hbox {}
    [:button {:label "step 1" :data-on:click "(set! $step 1)"}]
    [:button {:label "step 5" :data-on:click "(set! $step 5)"}]
    [:button {:id "add" :label "add"
              :data-indicator:saving ""
              :data-attr:sensitive "(not $saving)"
              :data-on:click__throttle.250ms "(POST \"/add\")"}]]])

(defn- elements-event
  "A datastar-patch-elements event patching `el` by its id."
  [el]
  (datastar/patch-elements-event (json/write-str el) nil "outer"))

(defn- signals-event [m]
  (str "event: datastar-patch-signals\ndata: signals " (json/write-str m) "\n\n"))

(defn- stream
  "An SSE response sending `events` (formatted strings), then ending."
  [& events]
  (let [ch (async/chan (count events))]
    (doseq [e events] (async/put! ch e))
    (async/close! ch)
    (sse/event-response ch)))

(defn- routes [{:keys [request-method uri] :as req}]
  (case [request-method uri]
    [:get "/"] (stream (elements-event (page @count*)))
    [:post "/add"] (let [step (long (or (:step (:jolt.datastar/signals req)) 1))
                         n (swap! count* + step)]
                     (stream (elements-event (count-label n))
                             (signals-event {:total n})))
    {:status 404 :body "not found"}))

(def handler
  "The app: routes behind glimmer-datastar's JSON signal parsing."
  (datastar/wrap-signals routes))

(defn -main [& [port]]
  (let [port (if port (Long/parseLong port) 3000)]
    (adapter/run-server handler {:port port})
    (println (str "json-counter on http://localhost:" port "/"))
    @(promise)))
