(ns jolt.datastar.client.test-backend
  "An in-memory glimmer backend: a widget is an atom {:id :tag :props :children},
  so tests can read the drawn tree and tell a reused widget (same :id) from a
  new one."
  (:require [glimmer.backend :as b]
            [glimmer.core :as ui]))

(defonce ^:private ids (atom 0))

(def backend
  {:name :test
   :create! (fn [tag props] (atom {:id (swap! ids inc) :tag tag :props props :children []}))
   :apply-props! (fn [_ w props] (swap! w update :props merge props))
   :append-child! (fn [_ p c] (swap! p update :children conj c))
   :remove-child! (fn [_ p c] (swap! p update :children (fn [cs] (vec (remove #(identical? % c) cs)))))
   :replace-child! (fn [_ p o n] (swap! p update :children (fn [cs] (mapv #(if (identical? % o) n %) cs))))
   :reorder-child! (fn [_ p c sib]
                     (swap! p update :children
                            (fn [cs] (let [without (vec (remove #(identical? % c) cs))
                                           i (if sib (inc (.indexOf without sib)) 0)]
                                       (vec (concat (subvec without 0 i) [c] (subvec without i)))))))})

(defn mount
  "Install the backend and mount `hiccup` into a fresh root; returns the root."
  [hiccup]
  (b/register! backend)
  (let [root (atom {:id 0 :tag :root :props {} :children []})]
    (ui/mount root :root hiccup)
    root))

(defn tree
  "The drawn tree under `w` as [tag props & children], props without handlers."
  [w]
  (let [{:keys [tag props children]} @w]
    (into [tag (into {} (remove (fn [[_ v]] (fn? v))) props)] (map tree) children)))

(defn find-by
  "The first widget under `w` whose props match `pred`."
  [w pred]
  (if (pred (:props @w)) w (some #(find-by % pred) (:children @w))))

(defn by-id [w id] (find-by w #(= id (:id %))))
