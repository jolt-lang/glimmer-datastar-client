(ns json-counter.app
  "The thin client for json-counter.server, on any glimmer backend: the same
  code draws with GTK or in the terminal.

      jolt -M:examples -m json-counter.app gtk [http://localhost:3000/]
      jolt -M:examples -m json-counter.app tui [http://localhost:3000/]"
  (:require [glimmer.core :as ui]
            [jolt.datastar.client :as ds]
            [jolt.datastar.client.codec :as codec]))

(defn -main [& [backend url]]
  ;; requiring a backend installs it; nothing else names a toolkit
  (require (symbol (str "glimmer-" (or backend "gtk") ".core")))
  (let [c (ds/client {:url (or url "http://localhost:3000/")
                      :codec codec/json
                      :root [:vbox {:id "app"} [:label {:label "connecting..."}]]})]
    (ds/connect! c)
    (ui/run (fn [] (ds/view c)) :title "json counter" :width 360 :height 160)))
