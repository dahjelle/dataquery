(ns datascript.core
  (:require-macros
    [datascript :refer [combine-cmp case-tree]]))

(defprotocol ISearch
  (-search [data pattern callback])
  (add-record [data e a v])
  (from-index [data index search callback]))

(defrecord DB [eav aev ave]
  Object
  (toString [this]
    (pr-str* this))

  ISearch
  (-search [db [e a v] callback]
    (case-tree [e a (some? v)] [
      (from-index db "eav" [e a v] callback)         ;; e a v
      (from-index db "eav" [e a nil] callback)       ;; e a _
      (from-index db "eav" [e nil nil] (fn [data]
        (callback (filter #(= v (.-v %)) data))))    ;; e _ v
      (from-index db "eav" [e nil nil] callback)     ;; e _ _
      (from-index db "ave" [a v nil] callback)       ;; _ a v
      (from-index db "ave" [a nil nil] callback)     ;; _ a _
      (callback (filter #(= v (.-v %)) eav))         ;; _ _ v
      (callback "eav")]))                            ;; _ _ _
  (add-record [_ e a v]
    (DB.
      (assoc eav (mapv str [e a v]) (to-array [e a v]))
      (assoc aev (mapv str [a e v]) (to-array [e a v]))
      (assoc ave (mapv str [a v e]) (to-array [e a v]))))
  (from-index [db index search callback]
    (callback (let [search-start search
                    search-stop  (mapv #(if (nil? %) "\uffff" %) search)
                    index        ((keyword index) db)]
      (vec (vals (subseq index >= search-start <= search-stop)))))))

(defn init-db []
  (DB. (sorted-map) (sorted-map) (sorted-map)))

