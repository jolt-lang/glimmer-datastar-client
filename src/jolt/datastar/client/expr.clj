(ns jolt.datastar.client.expr
  "Datastar's expressions, as EDN. A server writes them in the element data it
  sends, where Datastar writes JavaScript in data-* attributes:

      Datastar                          here
      data-text=\"$count\"                :data-text $count
      data-show=\"$open\"                 :data-show $open
      data-on:click=\"$count++\"          :data-on {:click (swap! $count inc)}
      data-on:click=\"@post('/save')\"    :data-on {:click (POST \"/save\")}
      $form.name                        $form.name

  The language is data, interpreted by this namespace, and has no way to reach
  the host: no interop, no eval, no I/O but the server actions. That keeps it to
  what Datastar uses expressions for, presentation and local behaviour (a tab, a
  menu, a counter, an animation), and keeps the application's logic on the
  server.

  Reading
    $a.b              the signal at [:a :b], read reactively
    evt               the event's value (an entry's text for :change), else nil
    el                the element's id
    literals          numbers, strings, keywords, booleans, nil; vectors and
                      maps of expressions
  Forms
    (if c a b) (when c ...) (when-not c ...) (cond ...) (case x ...) (do ...)
    (let [x e ...] ...) (and ...) (or ...) (not x)
    (peek $a)         read without subscribing (Datastar's @peek)
    pure functions    see `functions`
  Actions (only where the expression is an action: data-on, data-init,
  data-effect, data-on-interval, data-on-signal-patch)
    (set! $a v) (swap! $a f args...) (toggle! $a)
    (set-all! \"re\" v) (toggle-all! \"re\")      Datastar's @setAll, @toggleAll
    (animate! $a {:to n :ms 300 :ease :in-out}) tween a number signal
    (GET url opts?) (POST ...) (PUT ...) (PATCH ...) (DELETE ...)
                      a request to the server, with the signals (Datastar's
                      @get ...); opts :include / :exclude regex strings filter
                      them, as filterSignals does

  An expression may also be a string of that syntax, which is how one travels
  in a format with no symbols or lists, such as JSON:
  \"data-on\": {\"click\": \"(swap! $count inc)\"}. A string is source code
  only at the top: inside a form, \"text\" is a string.

  `compile-expr` turns a form into a fn of an env once; `evaluate` runs it. An
  env is a map:
    :signals   the signal store (jolt.datastar.client.signals)
    :evt :el   the event value and element id
    :action?   true where actions are allowed
    :fetch     (fn [request]) performs a server action; request is
               {:method :url :signals :opts :el}
    :timer     (fn [ms thunk]) runs thunk after ms on the UI thread and returns
               a fn that cancels it; used by animate!
    :now       (fn []) the time in ms; used by animate!"
  (:refer-clojure :exclude [compile])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [glimmer.ratom :as r]
            [jolt.datastar.client.signals :as sig]))

;; --- the functions an expression may call --------------------------------------

(defn- parse-num [s]
  (cond (number? s) s
        (string? s) (let [t (str/trim s)]
                      (try (if (re-find #"[.eE]" t) (Double/parseDouble t) (Long/parseLong t))
                           (catch Exception _ nil)))
        :else nil))

(def functions
  "The pure functions an expression may call, by symbol."
  {'+ + '- - '* * '/ / 'quot quot 'rem rem 'mod mod 'inc inc 'dec dec
   'max max 'min min 'abs (fn [x] (if (neg? x) (- x) x))
   'round (fn [x] (Math/round (double x))) 'floor (fn [x] (Math/floor (double x)))
   'ceil (fn [x] (Math/ceil (double x)))
   '= = 'not= not= '< < '> > '<= <= '>= >=
   'zero? zero? 'pos? pos? 'neg? neg? 'even? even? 'odd? odd?
   'nil? nil? 'some? some? 'empty? empty? 'true? true? 'false? false?
   'number? number? 'string? string? 'map? map? 'vector? vector?
   'not not 'identity identity
   'str str 'subs subs 'count count 'format format
   'upper-case str/upper-case 'lower-case str/lower-case 'trim str/trim
   'blank? str/blank? 'includes? str/includes? 'starts-with? str/starts-with?
   'ends-with? str/ends-with? 'join str/join 'split (fn [s re] (str/split s (re-pattern re)))
   'replace (fn [s match rep] (str/replace s match rep))
   'parse-number parse-num
   'get get 'get-in get-in 'assoc assoc 'assoc-in assoc-in 'dissoc dissoc
   'update (fn [m k f & args] (apply update m k f args))
   'conj conj 'disj disj 'contains? contains? 'merge merge
   'first first 'second second 'last last 'rest (comp vec rest) 'nth nth
   'take (comp vec take) 'drop (comp vec drop) 'reverse (comp vec reverse)
   'sort (comp vec sort) 'distinct (comp vec distinct)
   'vec vec 'set set 'keys (comp vec keys) 'vals (comp vec vals)
   'range (comp vec range) 'concat (comp vec concat)
   'keyword keyword 'name name})

(def ^:private action-forms
  '#{set! swap! toggle! set-all! toggle-all! animate! GET POST PUT PATCH DELETE})

;; --- compiling -----------------------------------------------------------------

(defn- fail [msg form]
  (throw (ex-info (str "datastar expression: " msg) {:form form})))

(defn signal-symbol?
  "True for a symbol naming a signal: $name, $a.b."
  [x]
  (and (symbol? x) (nil? (namespace x)) (str/starts-with? (name x) "$") (> (count (name x)) 1)))

(declare compile)

(defn- compile-body
  "A fn of env running the compiled `forms` in order, returning the last."
  [forms locals]
  (let [cs (mapv #(compile % locals) forms)]
    (case (count cs)
      0 (fn [_] nil)
      1 (first cs)
      (fn [env] (reduce (fn [_ c] (c env)) nil cs)))))

(defn- require-action [env form]
  (when-not (:action? env)
    (fail (str "(" (first form) " ...) changes state, and is only allowed in an action "
               "(data-on, data-init, data-effect, data-on-interval)")
          form)))

(defn- signal-path [form x]
  (when-not (signal-symbol? x)
    (fail (str "expected a signal like $name, got " (pr-str x)) form))
  (sig/path x))

(defn- compile-fetch [method [_ url-form opts-form :as form] locals]
  (let [url (compile url-form locals)
        opts (if opts-form (compile opts-form locals) (constantly nil))]
    (fn [env]
      (require-action env form)
      (let [o (or (opts env) {})
            filters {:include (some-> (:include o) re-pattern)
                     :exclude (some-> (:exclude o) re-pattern)}]
        (when-let [fetch (:fetch env)]
          (fetch {:method method :url (url env) :el (:el env)
                  :signals (sig/snapshot (:signals env) filters)
                  :opts (dissoc o :include :exclude)}))
        nil))))

(def ^:private easings
  {:linear  identity
   :in      (fn [t] (* t t))
   :out     (fn [t] (- 1 (* (- 1 t) (- 1 t))))
   :in-out  (fn [t] (if (< t 0.5) (* 2 t t) (- 1 (/ (* (- 2 (* 2 t)) (- 2 (* 2 t))) 2))))})

(defonce ^:private animations (atom {}))   ; [store path] -> cancel fn

(defn- animate!
  "Tween the number signal at `p` to (:to spec) over (:ms spec), one step per
  frame. A new animation of the same signal replaces a running one."
  [env p {:keys [to ms ease frame-ms] :or {ms 300 ease :in-out frame-ms 16}}]
  (let [s (:signals env)
        timer (or (:timer env) (fail "animate! needs a :timer" nil))
        now (or (:now env) #(System/currentTimeMillis))
        k [s p]
        from (double (or (sig/peek-value s p) 0))
        start (now)
        f (or (easings ease) (fail (str "unknown easing " ease) ease))
        cancel (atom nil)
        step (fn step []
               (let [t (min 1.0 (/ (- (now) start) (max 1 ms)))
                     v (if (>= t 1.0) to (+ from (* (- to from) (f t))))]
                 (sig/set-value! s p v)
                 (if (< t 1.0)
                   (reset! cancel (timer frame-ms step))
                   (swap! animations #(if (= (get % k) cancel) (dissoc % k) %)))))]
    (when-let [running (get @animations k)] (when-let [c @running] (c)))
    (swap! animations assoc k cancel)
    (step)
    nil))

(defn- compile-seq [form locals]
  (let [[head & args] form]
    (case head
      if (let [[c a b] (map #(compile % locals) args)
               b (or b (constantly nil))]
           (fn [env] (if (c env) (a env) (b env))))
      when (let [c (compile (first args) locals) body (compile-body (rest args) locals)]
             (fn [env] (when (c env) (body env))))
      when-not (let [c (compile (first args) locals) body (compile-body (rest args) locals)]
                 (fn [env] (when-not (c env) (body env))))
      do (compile-body args locals)
      and (let [cs (mapv #(compile % locals) args)]
            (fn [env] (reduce (fn [_ c] (let [v (c env)] (if v v (reduced v)))) true cs)))
      or (let [cs (mapv #(compile % locals) args)]
           (fn [env] (reduce (fn [_ c] (let [v (c env)] (if v (reduced v) v))) nil cs)))
      cond (let [pairs (mapv (fn [[t e]] [(if (= :else t) (constantly true) (compile t locals))
                                          (compile e locals)])
                             (partition 2 args))]
             (fn [env] (first (some (fn [[t e]] (when (t env) [(e env)])) pairs))))
      case (let [x (compile (first args) locals)
                 clauses (rest args)
                 default (when (odd? (count clauses)) (compile (last clauses) locals))
                 table (into {} (map (fn [[k e]] [k (compile e locals)]) (partition 2 clauses)))]
             (fn [env] (let [v (x env)]
                         (if-let [e (get table v)] (e env) (when default (default env))))))
      let (let [[bindings & body] args
                _ (when-not (and (vector? bindings) (even? (count bindings)))
                    (fail "let takes a vector of bindings" form))
                [steps locals'] (reduce (fn [[steps ls] [sym e]]
                                          (when-not (and (symbol? sym) (not (signal-symbol? sym)))
                                            (fail (str "can't bind " (pr-str sym)) form))
                                          [(conj steps [sym (compile e ls)]) (conj ls sym)])
                                        [[] locals] (partition 2 bindings))
                b (compile-body body locals')]
            (fn [env] (b (reduce (fn [env [sym c]] (assoc-in env [:locals sym] (c env))) env steps))))
      peek (let [p (signal-path form (first args))]
             (fn [env] (sig/peek-value (:signals env) p)))
      set! (let [p (signal-path form (first args)) v (compile (second args) locals)]
             (fn [env] (require-action env form) (sig/set-value! (:signals env) p (v env)) nil))
      swap! (let [p (signal-path form (first args))
                  f (or (functions (second args)) (fail (str "unknown function " (second args)) form))
                  more (mapv #(compile % locals) (drop 2 args))]
              (fn [env]
                (require-action env form)
                (let [s (:signals env)]
                  (sig/set-value! s p (apply f (sig/peek-value s p) (map #(% env) more))))
                nil))
      toggle! (let [p (signal-path form (first args))]
                (fn [env] (require-action env form)
                  (let [s (:signals env)] (sig/set-value! s p (not (sig/peek-value s p)))) nil))
      set-all! (let [re (compile (first args) locals) v (compile (second args) locals)]
                 (fn [env] (require-action env form)
                   (let [s (:signals env) x (v env)]
                     (doseq [p (vec (sig/paths s (some-> (re env) re-pattern)))] (sig/set-value! s p x)))
                   nil))
      toggle-all! (let [re (compile (first args) locals)]
                    (fn [env] (require-action env form)
                      (let [s (:signals env)]
                        (doseq [p (vec (sig/paths s (some-> (re env) re-pattern)))]
                          (sig/set-value! s p (not (sig/peek-value s p)))))
                      nil))
      animate! (let [p (signal-path form (first args)) spec (compile (second args) locals)]
                 (fn [env] (require-action env form) (animate! env p (spec env))))
      GET (compile-fetch :get form locals)
      POST (compile-fetch :post form locals)
      PUT (compile-fetch :put form locals)
      PATCH (compile-fetch :patch form locals)
      DELETE (compile-fetch :delete form locals)
      (if-let [f (and (symbol? head) (functions head))]
        (let [cs (mapv #(compile % locals) args)]
          (fn [env] (apply f (map #(% env) cs))))
        (fail (str "unknown form " (pr-str head)) form)))))

(defn- compile
  [form locals]
  (cond
    (signal-symbol? form) (let [p (sig/path form)] (fn [env] (sig/value (:signals env) p)))
    (symbol? form) (cond
                     (contains? locals form) (fn [env] (get-in env [:locals form]))
                     (= 'evt form) (fn [env] (:evt env))
                     (= 'el form) (fn [env] (:el env))
                     :else (fail (str "unknown symbol " form) form))
    (seq? form) (if (empty? form) (constantly ()) (compile-seq form locals))
    (vector? form) (let [cs (mapv #(compile % locals) form)] (fn [env] (mapv #(% env) cs)))
    (map? form) (let [cs (mapv (fn [[k v]] [(compile k locals) (compile v locals)]) form)]
                  (fn [env] (into {} (map (fn [[k v]] [(k env) (v env)])) cs)))
    :else (constantly form)))

;; --- public --------------------------------------------------------------------

(defonce ^:private cache (atom {}))
(def ^:private cache-limit 4096)

(defn read-expr
  "The form in expression source `s`. Reads one EDN form with no tags, so the
  source can say nothing the forms above cannot."
  [s]
  (try
    (edn/read-string {:readers {} :default (fn [tag _] (fail (str "no tags allowed: #" tag) s))} s)
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Exception e (fail (str "can't read " (pr-str s) ": " (ex-message e)) s))))

(defn compile-expr
  "The fn of an env that evaluates expression `form`: an EDN form, or a string
  holding one. Compiled once per distinct form; a form that is not valid throws
  ex-info naming it."
  [form]
  (or (get @cache form)
      (let [c (compile (if (string? form) (read-expr form) form) #{})]
        (swap! cache (fn [m] (assoc (if (>= (count m) cache-limit) {} m) form c)))
        c)))

(defn evaluate
  "Evaluate `form` (or a fn compile-expr made) in `env`."
  [form env]
  ((if (fn? form) form (compile-expr form)) env))

(defn action?
  "True when `form` contains a form that changes state or calls the server."
  [form]
  (boolean (some #(and (seq? %) (contains? action-forms (first %)))
                 (tree-seq coll? seq form))))

(defn untracked
  "Run (f) without subscribing whatever is rendering to the cells it reads."
  [f]
  (binding [r/*current-watcher* nil] (f)))
