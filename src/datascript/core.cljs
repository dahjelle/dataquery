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


(defn- slice-it [index search callback]
  (callback (let [search-start search
        search-stop  (mapv #(if (nil? %) "\uffff" %) search)]
    (vec (vals (subseq index >= search-start <= search-stop))))))

(defprotocol ISearch
  (-search [data pattern callback]))

(defprotocol Idb
  (add-record [data e a v]))

(defrecord DB [eav aev ave]
  Object
  (toString [this]
    (pr-str* this))

  ISearch
  (-search [_ [e a v] callback]
    (case-tree [e a (some? v)] [
      (slice-it eav [e a v] callback)                ;; e a v
      (slice-it eav [e a nil] callback)              ;; e a _
      (slice-it eav [e nil nil] (fn [data]
        (callback (filter #(= v (.-v %)) data))))    ;; e _ v
      (slice-it eav [e nil nil] callback)            ;; e _ _
      (slice-it ave [a v nil] callback)              ;; _ a v
      (slice-it ave [a nil nil] callback)            ;; _ a _
      (callback (filter #(= v (.-v %)) eav))         ;; _ _ v
      (callback eav)]))                              ;; _ _ _

  Idb
  (add-record [_ e a v]
    (DB.
      (assoc eav [e a v] [e a v])
      (assoc aev [a e v] [e a v])
      (assoc ave [a v e] [e a v]))))

(defn init-db []
  (DB. (sorted-map) (sorted-map) (sorted-map)))

