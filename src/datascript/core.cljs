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

(defn- slice-by-datom [index search])

(defprotocol ISearch
  (-search [data pattern callback]))

(defrecord DB [schema eavt aevt avet max-eid max-tx]
  Object
  (toString [this]
    (pr-str* this))

  ISearch
  (-search [_ [e a v tx] callback]
    (callback (case-tree [e a (some? v) tx] [
      (slice-by-datom eavt (Datom. e a v tx nil))                 ;; e a v tx
      (slice-by-datom eavt (Datom. e a v nil nil))                ;; e a v _
      (->> (slice-by-datom eavt (Datom. e a nil nil nil))         ;; e a _ tx
           (filter #(= tx (.-tx %))))
      (slice-by-datom eavt (Datom. e a nil nil nil))              ;; e a _ _
      (->> (slice-by-datom eavt (Datom. e nil nil nil nil))       ;; e _ v tx
           (filter #(and (= v (.-v %)) (= tx (.-tx %)))))
      (->> (slice-by-datom eavt (Datom. e nil nil nil nil))       ;; e _ v _
           (filter #(= v (.-v %))))
      (->> (slice-by-datom eavt (Datom. e nil nil nil nil))       ;; e _ _ tx
           (filter #(= tx (.-tx %))))
      (slice-by-datom eavt (Datom. e nil nil nil nil))            ;; e _ _ _
      (->> (slice-by-datom avet (Datom. nil a v nil nil))         ;; _ a v tx
           (filter #(= tx (.-tx %))))
      (slice-by-datom avet (Datom. nil a v nil nil))              ;; _ a v _
      (->> (slice-by-datom avet (Datom. nil a nil nil nil))       ;; _ a _ tx
           (filter #(= tx (.-tx %))))
      (slice-by-datom avet (Datom. nil a nil nil nil))            ;; _ a _ _
      (filter #(and (= v (.-v %)) (= tx (.-tx %))) eavt) ;; _ _ v tx
      (filter #(= v (.-v %)) eavt)                       ;; _ _ v _
      (filter #(= tx (.-tx %)) eavt)                     ;; _ _ _ tx
      eavt]))))                                           ;; _ _ _ _