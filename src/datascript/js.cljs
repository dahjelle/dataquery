(ns datascript.js
  (:require
    [datascript :as d]
    [datascript.core :as dc]
    [cljs.reader]))

;; Public API

(defn main [])

; required even if we don't use a main
(set! *main-cli-fn* main)

; export this ns as a part of nodejs module
(aset js/exports "db" (fn [meta search-eav search-ave]
  (dc/DB.
      meta
      (fn [search callback]
        (search-eav (to-array search) callback))
      (fn [search callback]
        (search-ave (to-array search) callback)))))

(aset js/exports "q" (fn [query callback & sources]
  (let [query   (cljs.reader/read-string query)]
    (apply d/q query #(->> (for [tuple %1]
                              (into-array tuple))
                          (into-array)
                          (callback)) sources))))