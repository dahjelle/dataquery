(ns datascript.core
  (:require-macros
    [datascript :refer [combine-cmp case-tree]]))

(defprotocol ISearch
  (-search [data pattern callback]))

(defrecord DB [db search-eav search-ave]
  Object
  (toString [this]
    (pr-str* this))

  ISearch
  (-search [db [e a v] callback]
    (case-tree [e a (some? v)] [
      (search-eav [e a v] callback)                  ;; e a v
      (search-eav [e a nil] callback)                ;; e a _
      (search-eav [e nil nil] (fn [data]
        (callback (filter #(= v (.-v %)) data))))    ;; e _ v
      (search-eav [e nil nil] callback)              ;; e _ _
      (search-ave [a v nil] callback)                ;; _ a v
      (search-ave [a nil nil] callback)              ;; _ a _
      (callback (filter #(= v (.-v %)) "eav"))       ;; _ _ v FIXME!
      (callback "eav")]))                            ;; _ _ _ FIXME!
)

