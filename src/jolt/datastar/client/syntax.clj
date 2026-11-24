(ns jolt.datastar.client.syntax
  "Datastar's attribute syntax, read into the map form jolt.datastar.client.attrs
  works on. A server can write attributes exactly as Datastar's own SDKs do:

      \"data-on:click__debounce.300ms\": \"(POST \\\"/search\\\")\"
      \"data-signals:first-name\": \"\\\"ada\\\"\"      ; $firstName, by Datastar's casing
      \"data-bind:query\": \"\"
      \"data-on-interval__duration.500ms\": \"(swap! $ticks inc)\"
      \"data-attr:sensitive\": \"(not $busy)\"

  and the element store turns each into the canonical form (`:data-on {:click
  {:do ... :debounce 300}}` ...), merging with any map-form props already there.
  The rules are Datastar's (library/src/engine/engine.ts): a name splits on
  `__` into the base and its modifiers, the base at its first `:` into the
  plugin and its key, and a modifier on `.` into its name and tags. Every value
  is an expression, as in Datastar.

  Keys that name signals (data-signals:, data-computed:, data-bind:,
  data-indicator:) are camelCased unless a `__case.kebab|snake|pascal` modifier
  says otherwise, as Datastar's are. Modifiers that only mean something in a
  browser (__window, __outside, __prevent, __stop, __passive, __capture,
  __viewtransition) are accepted and ignored."
  (:require [clojure.string :as str]))

;; --- names -----------------------------------------------------------------------

(defn parse-name
  "{:plugin :key :mods {name #{tags}}} for attribute name `n` (a string or
  keyword), or nil when it is not a data-* name."
  [n]
  (let [n (if (keyword? n) (subs (str n) 1) (str n))]
    (when (str/starts-with? n "data-")
      (let [[base & mods] (str/split n #"__")
            i (str/index-of base ":")
            [plugin k] (if i [(subs base 0 i) (subs base (inc i))] [base nil])]
        {:plugin plugin
         :key (not-empty k)
         :mods (into {} (map (fn [m] (let [[label & tags] (str/split m #"\.")]
                                       [label (set (map str/lower-case tags))])))
                     mods)}))))

(defn- camel [s] (str/replace s #"-([a-z])" (fn [[_ c]] (str/upper-case c))))

(defn cased
  "Signal name `s` cased by the __case modifier, camel by default."
  [s mods]
  (reduce (fn [s c]
            (case c
              "camel" (camel s)
              "kebab" s
              "snake" (str/replace s "-" "_")
              "pascal" (let [c (camel s)] (str (str/upper-case (subs c 0 1)) (subs c 1)))
              s))
          s (or (seq (get mods "case")) ["camel"])))

(defn ms
  "A duration from a modifier's tags, Datastar's tagToMs: 300ms, 2s, or a number."
  [tags default]
  (if-let [t (first (filter #(re-matches #"[0-9.]+(ms|s)?" %) tags))]
    (cond (str/ends-with? t "ms") (long (Double/parseDouble (subs t 0 (- (count t) 2))))
          (str/ends-with? t "s") (long (* 1000 (Double/parseDouble (subs t 0 (dec (count t))))))
          :else (long (Double/parseDouble t)))
    default))

;; --- one attribute -> canonical props ----------------------------------------------

(defn- on-spec
  "The {:do ...} spec for a data-on value with Datastar's timing modifiers."
  [v mods]
  (cond-> {:do v}
    (contains? mods "once") (assoc :once true)
    (contains? mods "delay") (assoc :delay (ms (get mods "delay") 0))
    (contains? mods "debounce") (assoc :debounce (ms (get mods "debounce") 0))
    (contains? mods "throttle") (assoc :throttle (ms (get mods "throttle") 0))))

(defn- preserve-list [v]
  (if (string? v) (mapv keyword (str/split (str/trim v) #"\s+")) v))

(defn- attribute
  "The canonical props for one Datastar-syntax attribute, or nil when `k` is
  already canonical or not Datastar's."
  [k v]
  (when-let [{:keys [plugin key mods]} (parse-name k)]
    (let [sig (when key (cased key mods))]
      (case plugin
        "data-on" (when key {:data-on {(keyword key) (on-spec v mods)}})
        "data-on-interval" (when (contains? mods "duration")
                             {:data-on-interval {:do v :ms (ms (get mods "duration") 1000)}})
        "data-on-signal-patch-filter" {:data-on-signal-patch {:filter v}}
        "data-signals" (let [k (if (contains? mods "ifmissing") :data-signals-if-missing :data-signals)]
                         (cond key {(keyword (str (name k) "-exprs")) {sig v}}
                               (contains? mods "ifmissing") {k v}
                               :else nil))
        "data-computed" (when key {:data-computed {sig v}})
        "data-attr" (when key {:data-attr {(keyword key) v}})
        ("data-bind" "data-indicator") (when key {(keyword plugin) sig})
        "data-preserve-attr" {:data-preserve-attr (preserve-list v)}
        nil))))

(defn- merge-canonical [a b]
  (merge-with (fn [x y]
                (if (and (map? x) (map? y))
                  (merge-with (fn [p q] (if (and (map? p) (map? q)) (merge p q) q)) x y)
                  y))
              a b))

(defn canonical
  "`props` with every Datastar-syntax attribute read into the canonical form;
  props that already are canonical are kept, and the two are merged."
  [props]
  (reduce-kv (fn [m k v]
               (if-let [c (attribute k v)]
                 (merge-canonical (dissoc m k) c)
                 (if (= :data-preserve-attr k) (assoc m k (preserve-list v)) m)))
             props props))
