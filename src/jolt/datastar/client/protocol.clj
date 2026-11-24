(ns jolt.datastar.client.protocol
  "Datastar's events, read off server-sent events. An SSE event's data is a set
  of `key value` lines (Datastar's framing); this turns one into a patch:

    {:type :patch-elements :selector \"#id\"|nil :mode :outer ... :elements [...]
     :use-view-transition bool}
    {:type :patch-signals :signals {...} :only-if-missing bool}

  The values are decoded by the codec, so nothing here knows the format."
  (:require [clojure.string :as str]))

(def modes #{:outer :inner :replace :prepend :append :before :after :remove})

(defn- data-lines
  "The data's `key value` lines as {key [value ...]}, in order."
  [data]
  (reduce (fn [m line]
            (let [i (str/index-of line " ")
                  k (if i (subs line 0 i) line)
                  v (if i (subs line (inc i)) "")]
              (update m k (fnil conj []) v)))
          {} (str/split-lines (or data ""))))

(defn- one [m k] (some-> (get m k) peek))
(defn- true-str? [s] (= "true" (some-> s str/trim str/lower-case)))

(defn read-event
  "The patch for SSE event `e` ({:event :data}), decoded with `codec`; nil for
  an event that is not Datastar's. Throws ex-info on a malformed one."
  [codec {:keys [event data]}]
  (case event
    "datastar-patch-elements"
    (let [m (data-lines data)
          mode (keyword (or (one m "mode") "outer"))
          selector (not-empty (one m "selector"))
          els (get m "elements")]
      (when-not (modes mode)
        (throw (ex-info (str "datastar: unknown patch mode " mode) {:mode mode})))
      (when (and selector (not (re-matches #"#[^\s#.\[\]:>+~,]+" selector)))
        (throw (ex-info (str "datastar: only #id selectors are supported, got " selector)
                        {:selector selector})))
      (when (and (nil? selector) (not (#{:outer :replace :remove} mode)))
        (throw (ex-info (str "datastar: mode " (name mode) " needs a selector") {:mode mode})))
      {:type :patch-elements
       :selector selector
       :mode mode
       :elements (if (seq els) ((:decode-elements codec) (str/join "\n" els)) [])
       :use-view-transition (true-str? (one m "useViewTransition"))})

    "datastar-patch-signals"
    (let [m (data-lines data)]
      {:type :patch-signals
       :signals (if-let [s (get m "signals")] ((:decode codec) (str/join "\n" s)) {})
       :only-if-missing (true-str? (one m "onlyIfMissing"))})

    nil))

(defn selector-id
  "The id in a `#id` selector."
  [selector]
  (when selector (subs selector 1)))
