(ns datascript.core
  (:require-macros
    [datascript :refer [combine-cmp case-tree]]))

(defrecord Datom [e a v tx added]
  Object
  (toString [this]
    (pr-str this)))

(extend-type Datom
  IHash
  (-hash [d] (or (.-__hash d)
                 (set! (.-__hash d)
                       (-> (hash (.-e d))
                           (hash-combine (hash (.-a d)))
                           (hash-combine (hash (.-v d)))))))
  IEquiv
  (-equiv [d o] (and (= (.-e d) (.-e o))
                     (= (.-a d) (.-a o))
                     (= (.-v d) (.-v o))))

  ISeqable
  (-seq [d] (list (.-e d) (.-a d) (.-v d) (.-tx d) (.-added d))))

;;;;;;;;;; Searching


(defprotocol ISearch
  (-search [data pattern callback])
  (add-record [data e a v])
  (slice-it [data index search callback]))

(defrecord DB [eav aev ave]
  Object
  (toString [this]
    (pr-str* this))

  ISearch
  (-search [db [e a v] callback]
    (case-tree [e a (some? v)] [
      (slice-it db "eav" [e a v] callback)                ;; e a v
      (slice-it db "eav" [e a nil] callback)              ;; e a _
      (slice-it db "eav" [e nil nil] (fn [data]
        (callback (filter #(= v (.-v %)) data))))    ;; e _ v
      (slice-it db "eav" [e nil nil] callback)            ;; e _ _
      (slice-it db "ave" [a v nil] callback)              ;; _ a v
      (slice-it db "ave" [a nil nil] callback)            ;; _ a _
      (callback (filter #(= v (.-v %)) eav))         ;; _ _ v
      (callback "eav")]))                              ;; _ _ _
  (add-record [_ e a v]
    (DB.
      (assoc eav (mapv str [e a v]) [e a v])
      (assoc aev (mapv str [a e v]) [e a v])
      (assoc ave (mapv str [a v e]) [e a v])))
  (slice-it [db index search callback]
    (callback (let [search-start search
                    search-stop  (mapv #(if (nil? %) "\uffff" %) search)
                    index        ((keyword index) db)]
      (vec (vals (subseq index >= search-start <= search-stop)))))))

(defn init-db []
  (DB. (sorted-map) (sorted-map) (sorted-map)))

