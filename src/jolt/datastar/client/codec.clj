(ns jolt.datastar.client.codec
  "How data goes on the wire. The client works on data only; a codec is the
  one place that reads and writes a format:

    {:content-type   \"application/edn\"
     :encode          (fn [signals]) -> string      signals for a request
     :decode          (fn [string])  -> data        a signals patch
     :decode-elements (fn [string])  -> [element ...] one or more elements}

  `edn` is the default; `json` is for a server that speaks JSON, as Datastar
  servers do (both parsers ship with jolt, so neither adds a dependency).
  Signals are kept JSON-compatible (`json-compatible`) whichever is used."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- problem [path msg] (throw (ex-info (str "signal " (str/join "." (map name path)) ": " msg)
                                          {:path path})))

(defn json-compatible
  "`signals` when every value in it survives a round trip through JSON: maps
  with keyword or string keys, vectors, strings, numbers, booleans and nil.
  Throws ex-info naming the first signal that does not."
  ([signals] (json-compatible [] signals) signals)
  ([path v]
   (cond
     (map? v) (doseq [[k x] v]
                (when-not (or (keyword? k) (string? k)) (problem path (str "key " (pr-str k) " is not a string")))
                (json-compatible (conj path (keyword (name k))) x))
     (vector? v) (doseq [x v] (json-compatible path x))
     (or (nil? v) (string? v) (boolean? v)) nil
     (number? v) (when (and (float? v) (or (Double/isNaN v) (Double/isInfinite v)))
                   (problem path "NaN and infinities have no JSON form"))
     :else (problem path (str (pr-str v) " has no JSON form")))))

(def edn
  "The EDN codec. Reading allows no tags but EDN's built-in #inst and #uuid, so
  a server cannot make the client construct anything but plain data."
  {:content-type "application/edn"
   :encode (fn [signals] (pr-str (json-compatible signals)))
   :decode (fn [s] (edn/read-string {:readers {} :default (fn [tag _] (throw (ex-info (str "unknown tag #" tag) {})))} s))
   :decode-elements (fn [s] (edn/read-string {:readers {} :default (fn [tag _] (throw (ex-info (str "unknown tag #" tag) {})))}
                                             (str "[" s "\n]")))})

;; --- JSON ------------------------------------------------------------------------

(defn- json-element
  "Hiccup from JSON hiccup: an array whose first item is a string is an element
  with that tag; its children are converted in turn. Map keys are already
  keywords (read with :key-fn keyword)."
  [x]
  (if (and (vector? x) (string? (first x)))
    (into [(keyword (first x))] (map json-element) (rest x))
    x))

(def json
  "The JSON codec. Elements are JSON hiccup, `[\"vbox\", {\"id\": \"app\"}, ...]`,
  with expressions as strings (`\"data-on\": {\"click\": \"(swap! $n inc)\"}`).
  `elements` holds one element or an array of them. Object keys read as
  keywords."
  {:content-type "application/json"
   :encode (fn [signals] (json/write-str (json-compatible signals)))
   :decode (fn [s] (json/read-str s :key-fn keyword))
   :decode-elements (fn [s]
                      (let [v (json/read-str s :key-fn keyword)]
                        (if (and (vector? v) (string? (first v)))
                          [(json-element v)]
                          (mapv json-element v))))})
