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


(defn- slice-it [index search]
  (let [search-start search
        search-stop  (mapv #(if (nil? %) "\uffff" %) search)]
    (println "slice-it" index search search-start search-stop)
    (println "slice-it" (vals (subseq index >= search-start <= search-stop)))
    (vec (vals (subseq index >= search-start <= search-stop)))))

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
    (callback (case-tree [e a (some? v)] [
      (slice-it eav [e a v])                ;; e a v
      (slice-it eav [e a nil])              ;; e a _
      (->> (slice-it eav [e nil nil])       ;; e _ v
           (filter #(= v (.-v %))))
      (slice-it eav [e nil nil])            ;; e _ _
      (slice-it ave [a v nil])              ;; _ a v
      (slice-it ave [a nil nil])            ;; _ a _
      (filter #(= v (.-v %)) eav)           ;; _ _ v
      eav])))                               ;; _ _ _

  Idb
  (add-record [_ e a v]
    (DB.
      (assoc eav [e a v] [e a v])
      (assoc aev [a e v] [e a v])
      (assoc ave [a v e] [e a v]))))

(defn init-db []
  (DB. (sorted-map) (sorted-map) (sorted-map)))

