(ns jolt.datastar.client.signals
  "The client's signals: Datastar's reactive store, the state a page keeps
  between server round trips (what is typed into a field, which tab is open).

  Signals are nested maps, addressed by a path of keywords, `[:form :name]` for
  Datastar's `$form.name`. Each leaf is its own glimmer ratom, so reading
  `$form.name` subscribes to that signal alone: typing into one field re-renders
  only what reads it. A leaf is made the first time anything reads or writes it,
  so an expression can read a signal before the server has sent it and is told
  when it arrives.

  A nil leaf is an absent signal: the merge-patch rules (RFC 7386) delete a key
  by patching it to nil, and `snapshot` leaves nil out."
  (:require [clojure.string :as str]
            [glimmer.ratom :as r]))

(defn store
  "A fresh, empty signal store."
  []
  {:cells     (atom {})     ; path -> ratom, one per leaf ever touched
   :listeners (atom {})})   ; id -> (fn [patch]), told of every merge-patch

;; --- paths ---------------------------------------------------------------------

(defn path
  "A signal path from a keyword, symbol or string (`:form.name`, `$form.name`,
  `\"form.name\"`), or a vector of segments. A leading `$` is dropped."
  [x]
  (cond
    (vector? x) (mapv keyword x)
    :else (let [s (cond (keyword? x) (subs (str x) 1)
                        (symbol? x) (str x)
                        :else (str x))
                s (if (str/starts-with? s "$") (subs s 1) s)]
            (mapv keyword (str/split s #"\.")))))

(defn path-name
  "The dotted name of a signal path, Datastar's spelling: [:form :name] is
  \"form.name\"."
  [p]
  (str/join "." (map name p)))

;; --- leaves --------------------------------------------------------------------

(defn cell
  "The ratom holding the leaf signal at path `p`, made when first asked for."
  [s p]
  (let [p (path p)]
    (or (get @(:cells s) p)
        (get (swap! (:cells s) (fn [cs] (if (contains? cs p) cs (assoc cs p (r/atom nil))))) p))))

(defn- prefix? [pre p]
  (and (< (count pre) (count p)) (= pre (subvec p 0 (count pre)))))

(defn- below
  "The [path cell] pairs of the leaves strictly under `p`."
  [s p]
  (filter (fn [[q _]] (prefix? p q)) @(:cells s)))

(defn- nest
  "A nested map from [path value] pairs relative to `base`, leaving nils out."
  [base pairs]
  (reduce (fn [m [q v]]
            (if (nil? v) m (assoc-in m (subvec q (count base)) v)))
          {} pairs))

(defn value
  "The signal at path `p`, read reactively. A leaf's value, or for a path with
  signals under it, the nested map of them (subscribing to each); nil when
  there are none."
  [s p]
  (let [p (path p)
        under (below s p)]
    (if (seq under)
      (not-empty (nest p (map (fn [[q c]] [q @c]) under)))
      @(cell s p))))

(defn peek-value
  "The signal at path `p`, read without subscribing: Datastar's @peek."
  [s p]
  (let [p (path p)
        under (below s p)]
    (if (seq under)
      (not-empty (nest p (map (fn [[q c]] [q (r/-value c)]) under)))
      (r/-value (cell s p)))))

(defn- clear-under! [s p]
  (doseq [[_ c] (below s p)] (reset! c nil)))

(defn set-value!
  "Write the signal at path `p`. A map value replaces everything under `p` with
  its leaves; nil deletes the signal and everything under it."
  [s p v]
  (let [p (path p)]
    (if (map? v)
      (do (reset! (cell s p) nil)
          (clear-under! s p)
          (doseq [[k x] v] (set-value! s (conj p (keyword k)) x)))
      (do (clear-under! s p)
          (reset! (cell s p) v))))
  nil)

;; --- merge patch ---------------------------------------------------------------

(defn- present? [s p]
  (or (some? (r/-value (cell s p)))
      (some (fn [[_ c]] (some? (r/-value c))) (below s p))))

(defn merge-patch!
  "Apply `patch`, a nested map, by JSON merge-patch rules (RFC 7386), as
  datastar-patch-signals does: a map merges into the signals under its key, nil
  deletes, anything else replaces. With `:only-if-missing`, a leaf is written
  only when the signal is absent. Listeners (see on-patch!) are told after."
  ([s patch] (merge-patch! s patch nil))
  ([s patch {:keys [only-if-missing]}]
   (letfn [(walk [base m]
             (doseq [[k v] m
                     :let [p (conj base (keyword k))]]
               (cond
                 (map? v) (do (when (some? (r/-value (cell s p))) (reset! (cell s p) nil))
                              (walk p v))
                 (nil? v) (when-not only-if-missing
                            (reset! (cell s p) nil)
                            (clear-under! s p))
                 :else    (when-not (and only-if-missing (present? s p))
                            (clear-under! s p)
                            (reset! (cell s p) v)))))]
     (walk [] patch))
   (doseq [f (vals @(:listeners s))] (f patch))
   nil))

(defn on-patch!
  "Call `f` with each merge-patch applied to `s` (Datastar's
  data-on-signal-patch). Returns a fn that stops it."
  [s f]
  (let [id (Object.)]
    (swap! (:listeners s) assoc id f)
    (fn [] (swap! (:listeners s) dissoc id) nil)))

;; --- patches -------------------------------------------------------------------

(defn flatten-patch
  "The [path value] leaves of a nested patch map, a nil value included."
  ([patch] (flatten-patch [] patch))
  ([base patch]
   (mapcat (fn [[k v]]
             (let [p (conj base (keyword k))]
               (if (and (map? v) (seq v)) (flatten-patch p v) [[p v]])))
           patch)))

;; --- what is sent to the server ------------------------------------------------

(defn- local?
  "Datastar's default exclude, /(^|\\.)_/: a segment starting with _ is local to
  the client and never sent."
  [p]
  (some #(str/starts-with? (name %) "_") p))

(defn snapshot
  "Every present signal as a nested map, read without subscribing. Signals with
  a path segment starting with _ are left out unless `:include-local` is set;
  `:include` and `:exclude` are regexes matched against the dotted name, as in
  Datastar's filterSignals."
  ([s] (snapshot s nil))
  ([s {:keys [include exclude include-local]}]
   (nest [] (for [[p c] @(:cells s)
                  :let [v (r/-value c) nm (path-name p)]
                  :when (and (some? v)
                             (or include-local (not (local? p)))
                             (or (nil? include) (re-find include nm))
                             (or (nil? exclude) (not (re-find exclude nm))))]
              [p v]))))

(defn paths
  "The paths of every present leaf whose dotted name matches `re` (all when
  nil), for @setAll / @toggleAll."
  [s re]
  (for [[p c] @(:cells s)
        :when (and (some? (r/-value c))
                   (or (nil? re) (re-find re (path-name p))))]
    p))
