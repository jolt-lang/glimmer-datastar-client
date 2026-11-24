(ns jolt.datastar.client.store
  "The element store: the page the server has sent, kept as data, drawn through
  glimmer, and patched by datastar-patch-elements.

  Every element with an :id lives in its own cell, holding the element with each
  id'd descendant replaced by a reference to that descendant's cell. Each cell
  is drawn by its own glimmer component, keyed by id. So:

  - a patch to #row-7 writes one cell, and only that component re-renders;
  - the morph is glimmer's reconciler: an element keeps its widget (and what
    the user typed into it) while its id, or for an unkeyed element its tag and
    order among its siblings, stays the same, which is how Datastar's morph
    matches elements;
  - an element dropped from the page drops its cell, stopping its lifetime
    behaviours (see jolt.datastar.client.attrs/mount!).

  Patches run where they are applied: on the UI thread, which client/apply!
  arranges. An element moved to a new parent keeps its cell, signals and
  behaviours, but gets a new widget: glimmer does not reparent widgets."
  (:require [glimmer.ratom :as r]
            [jolt.datastar.client.attrs :as attrs]
            [jolt.datastar.client.protocol :as protocol]
            [jolt.datastar.client.syntax :as syntax]))

(def ^:private root ::root)

(defn store
  "An empty element store; give it a page with mount-root!. `env` is the
  expression env its elements evaluate in (see jolt.datastar.client.expr):
  :signals, :fetch, :timer, :now, :on-error."
  [env]
  (let [s {:env env
           :cells (atom {})       ; id -> ratom of the normalized element
           :owner (atom {})       ; id -> the id of the cell referencing it
           :gens (atom {})        ; id -> generation, bumped by a replace
           :life (atom {})}]      ; [id path] -> {:attrs m :stop fn}
    (swap! (:cells s) assoc root (r/atom nil))
    s))

;; --- normalizing ------------------------------------------------------------------

(defn- ref? [x] (and (vector? x) (= ::ref (first x))))

(defn- parts
  "[tag props children] of element `el`."
  [el]
  (let [[tag & body] el
        props (when (map? (first body)) (first body))]
    [tag (or props {}) (vec (if props (rest body) body))]))

(defn- canonical
  "Element `el` (raw, as the server sent it) with Datastar-syntax attributes
  read into the map form (jolt.datastar.client.syntax)."
  [el]
  (let [[tag props children] (parts el)]
    (into [tag (syntax/canonical props)] children)))

(defn- element? [x] (and (vector? x) (keyword? (first x)) (not= ::ref (first x))))

(defn- id-of [x] (when (element? x) (some-> (second (parts x)) :id str)))

(declare put-element!)

(defn- normalize
  "Element content with each id'd element below put in its own cell (owned by
  `owner`) and replaced by a reference. Returns the normalized form."
  [s owner x]
  (cond
    (element? x)
    (if-let [id (id-of (canonical x))]
      (do (put-element! s owner id x) [::ref id (get @(:gens s) id 0)])
      (let [[tag props children] (parts (canonical x))]
        (into [tag props] (map #(normalize s owner %)) children)))
    (seq? x) (map #(normalize s owner %) x)
    :else x))

(defn- refs
  "The ids a normalized element references directly."
  [x]
  (cond (ref? x) [(second x)]
        (element? x) (mapcat refs (nth (parts x) 2))
        (seq? x) (mapcat refs x)
        :else nil))

(defn- lifecycle-attrs [props]
  (not-empty (select-keys props [:data-signals :data-signals-if-missing
                                 :data-signals-exprs :data-signals-if-missing-exprs :data-computed
                                 :data-effect :data-on-interval :data-on-signal-patch
                                 :data-init :data-indicator :data-bind])))

(defn- lifecycles
  "{path [tag props]} for the elements of normalized `x` (not crossing into
  referenced cells) that have lifetime behaviours."
  ([x] (lifecycles [] x))
  ([path x]
   (if (element? x)
     (let [[tag props children] (parts x)]
       (merge (when (lifecycle-attrs props) {path [tag props]})
              (apply merge (map-indexed (fn [i c] (lifecycles (conj path i) c)) children))))
     {})))

(defn- sync-lifecycles!
  "Start, restart or stop the lifetime behaviours of cell `id`'s elements to
  match its content `x`: an element whose lifecycle attributes changed is
  restarted, as Datastar re-runs a changed attribute."
  [s id x]
  (let [now (lifecycles x)
        before (into {} (filter (fn [[[i _] _]] (= i id))) @(:life s))]
    (doseq [[[_ path :as k] {:keys [attrs stop]}] before]
      (when (not= attrs (some-> (get now path) second lifecycle-attrs))
        (stop)
        (swap! (:life s) dissoc k)))
    (doseq [[path [tag props]] now
            :let [k [id path] la (lifecycle-attrs props)]
            :when (not= la (get-in @(:life s) [k :attrs]))]
      (swap! (:life s) assoc k {:attrs la :stop (attrs/mount! tag props (:env s))}))))

(defn- drop-cell!
  "Forget cell `id` and every cell below it: stop their behaviours and remove
  them, unless one was taken by another owner meanwhile."
  [s id]
  (when-let [c (get @(:cells s) id)]
    (let [below (refs @c)]
      (doseq [[k {:keys [stop]}] @(:life s) :when (= id (first k))]
        (stop) (swap! (:life s) dissoc k))
      (swap! (:cells s) dissoc id)
      (swap! (:owner s) dissoc id)
      (doseq [child below :when (= id (get @(:owner s) child))]
        (drop-cell! s child)))))

(def ^:private ^:dynamic *touched*
  "While a patch runs, a volatile of the cells it wrote, whose behaviours are
  synced when it ends, parents first."
  nil)

(defn- depth [s id] (loop [i id n 0] (if-let [o (get @(:owner s) i)] (recur o (inc n)) n)))

(defn- with-lifecycles
  "Run (f), then start and stop the behaviours of the cells it wrote, in
  document order by depth, as Datastar applies attributes parent first: a
  child's computed signal sees its parent's data-signals."
  [s f]
  (if *touched*
    (f)
    (let [touched (volatile! [])]
      (binding [*touched* touched] (f))
      (doseq [id (sort-by #(depth s %) (distinct @touched))
              :let [c (get @(:cells s) id)]
              :when c]
        (sync-lifecycles! s id @c)))))

(defn- set-cell!
  "Write normalized content `x` to cell `id`, dropping the cells it no longer
  references; its behaviours are synced when the patch ends."
  [s id x]
  (let [c (or (get @(:cells s) id)
              (get (swap! (:cells s) assoc id (r/atom nil)) id))
        old @c
        gone (remove (set (refs x)) (refs old))]
    (reset! c x)
    (doseq [g gone :when (= id (get @(:owner s) g))] (drop-cell! s g))
    (if *touched* (vswap! *touched* conj id) (sync-lifecycles! s id x))))

(defn- ignore-morph? [old new]
  (and (element? old) (element? new)
       (contains? (second (parts old)) :data-ignore-morph)
       (contains? (second (parts new)) :data-ignore-morph)))

(defn- preserve-attrs
  "`new`'s props, keeping the values `old` had for the props it names in
  :data-preserve-attr (a keyword or a collection of them)."
  [old new]
  (let [[tag props children] (parts new)
        keep (let [p (:data-preserve-attr props)] (if (keyword? p) [p] p))]
    (if (and (seq keep) (element? old))
      (into [tag (merge props (select-keys (second (parts old)) keep))] children)
      new)))

(defn- put-element!
  "Put element `el` (raw, with :id `id`) under owner `owner`."
  [s owner id el]
  (swap! (:owner s) assoc id owner)
  (let [el (canonical el)
        old (some-> (get @(:cells s) id) deref)]
    (when-not (ignore-morph? old el)
      (let [el (preserve-attrs old el)
            [tag props children] (parts el)]
        (set-cell! s id (into [tag props] (map #(normalize s id %)) children))))))

;; --- patching ---------------------------------------------------------------------

(defn- cell-of [s id] (get @(:cells s) id))

(defn- splice-in-owner!
  "Rewrite the reference to `id` in cell `owner` with (f ref) -> seq of
  normalized nodes."
  [s owner id f]
  (when-let [c (cell-of s owner)]
    (let [
          replace (fn walk [x]
                    (cond (and (ref? x) (= id (second x))) (f x)
                          (element? x) (let [[tag props children] (parts x)]
                                         [(into [tag props] (mapcat walk) children)])
                          :else [x]))]
      (set-cell! s owner (first (replace @c))))))

(defn- update-children!
  "Set cell `id`'s children to (f children), normalizing new ones into it."
  [s id f]
  (when-let [c (cell-of s id)]
    (let [[tag props children] (parts @c)]
      (set-cell! s id (into [tag props] (f children))))))

(defn- warn [s msg data]
  (if-let [f (get-in s [:env :on-error])]
    (f (ex-info msg data) nil)
    (binding [*out* *err*] (println "datastar client:" msg (pr-str data)))))

(defn- replace-by-id!
  "outer (morph) or replace (`fresh?`: new widgets, state dropped) of element
  `target` with `el`. Replacing it with an element of another id, or with none,
  swaps the reference in its owner, which drops the old cell."
  [s target el fresh?]
  (let [id (id-of el)
        owner (get @(:owner s) target)]
    (cond
      (nil? (cell-of s target)) (warn s "no element to patch" {:id target})
      (and (= id target) (not fresh?)) (put-element! s owner id el)
      :else (do (when fresh?
                  (swap! (:gens s) update target (fnil inc 0))
                  (when (= id target) (drop-cell! s target)))
                (splice-in-owner! s owner target (fn [_] [(normalize s owner el)]))))))

(declare patch*)

(defn patch!
  "Apply a :patch-elements patch (jolt.datastar.client.protocol/read-event)."
  [s patch]
  (with-lifecycles s #(patch* s patch)))

(defn- patch*
  [s {:keys [selector mode elements]}]
  (let [target (protocol/selector-id selector)
        new-nodes (fn [owner] (mapv #(normalize s owner %) elements))]
    (case mode
      (:outer :replace)
      (if target
        (when-let [el (first elements)] (replace-by-id! s target el (= mode :replace)))
        (doseq [el elements]
          (if-let [id (id-of el)]
            (replace-by-id! s id el (= mode :replace))
            (warn s "an element patched without a selector needs an :id" {:element el}))))

      :remove
      (doseq [id (if target [target] (keep id-of elements))]
        (if (cell-of s id)
          (splice-in-owner! s (get @(:owner s) id) id (fn [_] []))
          (warn s "no element to remove" {:id id})))

      :inner (if (cell-of s target)
               (update-children! s target (fn [_] (new-nodes target)))
               (warn s "no element to patch" {:id target}))
      :prepend (if (cell-of s target)
                 (update-children! s target (fn [cs] (into (new-nodes target) cs)))
                 (warn s "no element to patch" {:id target}))
      :append (if (cell-of s target)
                (update-children! s target (fn [cs] (into cs (new-nodes target))))
                (warn s "no element to patch" {:id target}))
      :before (if-let [owner (get @(:owner s) target)]
                (splice-in-owner! s owner target (fn [r] (conj (new-nodes owner) r)))
                (warn s "no element to patch" {:id target}))
      :after (if-let [owner (get @(:owner s) target)]
               (splice-in-owner! s owner target (fn [r] (into [r] (new-nodes owner))))
               (warn s "no element to patch" {:id target})))
    nil))

(defn mount-root!
  "Put `root-element` (which must have an :id) in the store as the page."
  [s root-element]
  (when-not (id-of root-element)
    (throw (ex-info "datastar client: the root element needs an :id" {:element root-element})))
  (with-lifecycles s #(set-cell! s root (normalize s root root-element))))

;; --- drawing ----------------------------------------------------------------------

(declare node keyed)

(defn- key-of
  "A reference's glimmer key: its id and generation, so a replace (a new
  generation) gets new widgets."
  [r]
  [(second r) (nth r 2 0)])

(defn- draw
  "Glimmer hiccup for normalized `x`, applying data-* attributes. `states` keeps
  each element's handler state (debounce, throttle, once) by path."
  [s states path x]
  (cond
    (ref? x) (with-meta [node s (second x)] {:key (key-of x)})
    (element? x)
    (let [[tag props children] (parts x)
          st (or (get @states path) (get (swap! states assoc path (atom {})) path))]
      (when-let [p (attrs/render-props tag props (:env s) st)]
        (into [tag p] (keyed s (keep-indexed (fn [i c] (draw s states (conj path i) c)) children)))))
    (seq? x) (map-indexed (fn [i c] (draw s states (conj path i) c)) x)
    :else x))

(defn- keyed
  "`children` (drawn hiccup) each with a :key, so glimmer always matches them
  the way Datastar's morph does: a cell's component by its id, an unkeyed
  element by its tag and how many of that tag come before it. Text is left as
  it is."
  [_ children]
  (first (reduce (fn [[out seen] c]
                   (cond
                     (some? (:key (meta c))) [(conj out c) seen]
                     (and (vector? c) (keyword? (first c)))
                     (let [n (get seen (first c) 0)]
                       [(conj out (with-meta c {:key [::auto (first c) n]})) (assoc seen (first c) (inc n))])
                     :else [(conj out c) seen]))
                 [[] {}] children)))

(defn- node
  "The component drawing cell `id`: re-renders when that cell, or a signal its
  expressions read, changes."
  [s id]
  (let [states (atom {})]
    ;; glimmer calls a Form-2 render fn with no args; s and id never change
    (fn []
      (when-let [c (cell-of s id)]
        (draw s states [] @c)))))

(defn view
  "The glimmer hiccup for the whole page: mount it with glimmer.core/mount."
  [s]
  [(fn [s] (some->> (cell-of s root) deref (draw s (atom {}) []))) s])

(defn element
  "The current normalized content of element `id` (for tests and tools)."
  [s id]
  (some-> (cell-of s id) deref))

(defn ids
  "Every id the store holds."
  [s]
  (set (remove #{root} (keys @(:cells s)))))
