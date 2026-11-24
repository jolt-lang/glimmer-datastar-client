(ns jolt.datastar.client.attrs
  "Datastar's data-* attributes, over the EDN elements a server sends. An
  element's props may carry, beside the backend's own props:

    :data-signals {...}            merge these signals in (data-signals); or an
                                   expression whose value is the map
    :data-signals-if-missing {...} the same, only where a signal is absent
    :data-computed {:name expr}    a read-only signal derived from others
    :data-text expr                the element's text (its :label, an entry's :text)
    :data-show expr                draw the element only while expr is truthy
    :data-attr {:prop expr}        any backend prop, from an expression; or one
                                   expression whose value is a map of props
    :data-bind :name               two-way: the element's value <-> $name
    :data-on {:click expr}         run an action on an event; an event's value
                                   is `evt`. With modifiers:
                                   {:click {:do expr :debounce ms :throttle ms
                                            :delay ms :once true}}
    :data-on-interval expr         run every second, or {:do expr :ms n}
    :data-on-signal-patch expr     run after each server signal patch, `evt`
                                   being the patch; or {:do expr :include re},
                                   or {:do expr :filter expr} where the filter
                                   evaluates to {:include re :exclude re}

  Datastar's own attribute names (data-on:click__debounce.300ms, ...) are read
  into these by jolt.datastar.client.syntax; keyed signals arrive as
  :data-signals-exprs {name expr}.
    :data-init expr                run once when the element appears
    :data-effect expr              run now and whenever a signal it reads changes
    :data-indicator :name          $name is true while a request this element
                                   made is running

  Expressions are jolt.datastar.client.expr's. Nothing here names a toolkit:
  which prop holds an element's text and value, and which event reports a
  change, is a table (`text-props`, `bindings`) a backend's vocabulary can
  extend.

  An element has two halves. `render-props` runs on every render, inside the
  glimmer component drawing the element, so the signals its expressions read
  re-render that element alone. `mount!` runs once when the element appears
  and returns a fn that undoes it (stops its intervals, effects and listeners),
  for whoever owns the element's lifetime to call when it goes."
  (:require [clojure.string :as str]
            [glimmer.ratom :as r]
            [jolt.datastar.client.expr :as expr]
            [jolt.datastar.client.signals :as sig]))

;; --- vocabulary --------------------------------------------------------------

(defonce ^{:doc "tag -> the prop :data-text sets. Tags not listed use :label."}
  text-props
  (atom {:entry :text :window :title}))

(defonce ^{:doc "tag -> how :data-bind connects to it: the value :prop, the :event
  reporting a change, and (:value evt current) the new value from that event."}
  bindings
  (atom {:entry       {:prop :text :event :on-change :value (fn [evt _] evt)}
         :checkbutton {:prop :active :event :on-toggled :value (fn [_ current] (not current))}}))

(defn register-text-prop! [tag prop] (swap! text-props assoc tag prop) nil)

(defn register-binding!
  "Teach :data-bind a tag: {:prop :event :value (fn [evt current])}."
  [tag spec]
  (swap! bindings assoc tag spec) nil)

(defn data-key? [k] (and (keyword? k) (str/starts-with? (name k) "data-")))

;; --- helpers -----------------------------------------------------------------

(defn- report! [env e form]
  (if-let [f (:on-error env)]
    (f e form)
    (binding [*out* *err*]
      (println "datastar client: expression failed:" (pr-str form) "-" (ex-message e)))))

(defn- run-action!
  "Evaluate action `form` untracked, reporting rather than throwing: a failing
  handler must not take the UI loop down."
  [env form evt]
  (try
    (expr/untracked #(expr/evaluate form (assoc env :action? true :evt evt)))
    (catch Exception e (report! env e form) nil)))

(defn- event-prop [event] (keyword (str "on-" (name event))))

(defn- with-indicator
  "`env` whose :fetch sets $sig true until the request's :done is called."
  [env sig-name]
  (if-let [fetch (and sig-name (:fetch env))]
    (let [s (:signals env) p (sig/path sig-name)]
      (assoc env :fetch (fn [req]
                          (sig/set-value! s p true)
                          (fetch (assoc req :done (fn []
                                                    (sig/set-value! s p false)
                                                    (when-let [d (:done req)] (d))))))))
    env))

(defn- spec-of [x] (if (and (map? x) (contains? x :do)) x {:do x}))

(defn- modified-handler
  "The event handler for `spec` ({:do form & modifiers}), keeping debounce,
  throttle and once state in `state` (an atom outliving renders) under `k`."
  [env state k {action :do :keys [debounce throttle delay once]}]
  (let [timer (:timer env)
        now (or (:now env) #(System/currentTimeMillis))
        fire (fn [evt]
               (when-not (and once (get-in @state [k :fired]))
                 (when once (swap! state assoc-in [k :fired] true))
                 (run-action! env action evt)))]
    (cond
      (and debounce timer)
      (fn [& args]
        (when-let [c (get-in @state [k :pending])] (c))
        (swap! state assoc-in [k :pending] (timer debounce #(do (swap! state update k dissoc :pending)
                                                                (fire (first args))))))
      throttle
      (fn [& args]
        (let [t (now) last (get-in @state [k :last])]
          (when (or (nil? last) (>= (- t last) throttle))
            (swap! state assoc-in [k :last] t)
            (fire (first args)))))
      (and delay timer)
      (fn [& args] (timer delay #(fire (first args))))
      :else
      (fn [& args] (fire (first args))))))

(defn- chain
  "A handler running `a` then `b` with the same args; either may be nil."
  [a b]
  (cond (and a b) (fn [& args] (apply a args) (apply b args))
        :else (or a b)))

;; --- per render --------------------------------------------------------------

(defn render-props
  "The props to hand the backend for element `tag` with `props`: :data-*
  applied and removed. nil when :data-show hides the element. `env` is an
  expression env (see jolt.datastar.client.expr) and `state` an atom kept for
  the element's lifetime, holding its debounce/throttle state."
  [tag props env state]
  (let [el (:id props)
        env (-> (assoc env :el el :action? false)
                (with-indicator (:data-indicator props)))
        eval* (fn [form] (expr/evaluate form env))]
    (when (or (not (contains? props :data-show)) (eval* (:data-show props)))
      (let [base (into {} (remove (fn [[k _]] (data-key? k))) props)
            with-text (if (contains? props :data-text)
                        (assoc base (get @text-props tag :label) (eval* (:data-text props)))
                        base)
            da (:data-attr props)
            with-attrs (if (or (nil? da) (map? da))
                         (reduce-kv (fn [m k form] (assoc m k (eval* form))) with-text da)
                         (merge with-text (eval* da)))
            bound (when-let [b (:data-bind props)]
                    (let [{:keys [prop event value] :as spec} (get @bindings tag)
                          _ (when-not spec
                              (throw (ex-info (str "datastar client: no :data-bind for " tag) {:tag tag})))
                          s (:signals env) p (sig/path b)]
                      {:prop prop :event event
                       :v (sig/value s p)
                       :handler (fn [& args]
                                  (sig/set-value! s p (value (first args) (sig/peek-value s p))))}))
            with-bind (if bound
                        (cond-> with-attrs (some? (:v bound)) (assoc (:prop bound) (:v bound)))
                        with-attrs)
            handlers (reduce-kv (fn [m event x]
                                  (let [pk (event-prop event)]
                                    (assoc m pk (modified-handler env state pk (spec-of x)))))
                                {} (:data-on props))
            handlers (if bound (update handlers (:event bound) #(chain (:handler bound) %)) handlers)]
        (merge with-bind handlers)))))

;; --- per element lifetime ----------------------------------------------------

(defn effect!
  "Run (f) now and again whenever a reactive cell it read changes, until the
  returned fn is called."
  [f]
  (let [stopped (atom false) w (atom nil) scope (atom nil)
        run (fn []
              (r/clear-subscriptions! @scope)
              (binding [r/*current-watcher* @w r/*watch-scope* @scope] (f)))]
    (reset! w (fn [_] (when-not @stopped (run))))
    (reset! scope (r/watch-scope @w))
    (run)
    (fn [] (reset! stopped true) (r/clear-subscriptions! @scope) nil)))

(defn- every!
  "Call (f) every `ms` through `timer` until the returned fn is called."
  [timer ms f]
  (let [cancel (atom nil) stopped (atom false)
        tick (fn tick [] (when-not @stopped (f) (when-not @stopped (reset! cancel (timer ms tick)))))]
    (reset! cancel (timer ms tick))
    (fn [] (reset! stopped true) (when-let [c @cancel] (c)) nil)))

(defn mount!
  "Start element `tag`'s lifetime behaviours from `props`: merge its signals,
  start its computed signals, effects, intervals and signal-patch listeners, run
  its init. Returns a fn that stops them all."
  [tag props env]
  (let [el (:id props)
        s (:signals env)
        env (-> (assoc env :el el)
                (with-indicator (:data-indicator props)))
        stops (atom [])
        stop! #(swap! stops conj %)]
    (let [value (fn [x] (if (map? x) x (expr/evaluate x (assoc env :action? false))))
          exprs (fn [m] (reduce-kv (fn [acc n form]
                                     (assoc-in acc (sig/path n) (expr/evaluate form (assoc env :action? false))))
                                   {} m))]
      (when-let [m (:data-signals props)] (sig/merge-patch! s (value m)))
      (when-let [m (:data-signals-exprs props)] (sig/merge-patch! s (exprs m)))
      (when-let [m (:data-signals-if-missing props)]
        (sig/merge-patch! s (value m) {:only-if-missing true}))
      (when-let [m (:data-signals-if-missing-exprs props)]
        (sig/merge-patch! s (exprs m) {:only-if-missing true})))
    (when-let [ind (:data-indicator props)] (sig/set-value! s (sig/path ind) false))
    ;; a bound signal starts from the element's own value when it has none
    (when-let [b (:data-bind props)]
      (when-let [{:keys [prop]} (get @bindings tag)]
        (let [p (sig/path b)]
          (when (and (nil? (sig/peek-value s p)) (some? (get props prop)))
            (sig/set-value! s p (get props prop))))))
    (doseq [[n form] (:data-computed props)]
      (let [p (sig/path n)]
        (stop! (effect! (fn []
                          (let [v (try (expr/evaluate form (assoc env :action? false))
                                       (catch Exception e (report! env e form) nil))]
                            (expr/untracked #(sig/set-value! s p v))))))))
    (when-let [form (:data-effect props)]
      (stop! (effect! (fn [] (try (expr/evaluate form (assoc env :action? true))
                                  (catch Exception e (report! env e form)))))))
    (when-let [x (:data-on-interval props)]
      (let [{action :do ms :ms :or {ms 1000}} (spec-of x)]
        (if-let [timer (:timer env)]
          (stop! (every! timer ms #(run-action! env action nil)))
          (throw (ex-info "datastar client: :data-on-interval needs a :timer" {:el el})))))
    (when-let [x (:data-on-signal-patch props)]
      (let [{action :do include :include exclude :exclude f :filter} (spec-of x)
            fv (when f (if (map? f) f (expr/evaluate f (assoc env :action? false))))
            inc-re (some-> (or include (:include fv)) re-pattern)
            exc-re (some-> (or exclude (:exclude fv)) re-pattern)
            wanted? (fn [n] (and (or (nil? inc-re) (re-find inc-re n))
                                 (or (nil? exc-re) (not (re-find exc-re n)))))]
        (stop! (sig/on-patch! s (fn [patch]
                                  (when (some wanted? (map (comp sig/path-name first)
                                                           (sig/flatten-patch patch)))
                                    (run-action! env action patch)))))))
    (when-let [form (:data-init props)]
      (run-action! env form nil))
    (fn [] (doseq [f @stops] (f)) nil)))
