(ns datascript.js
  (:require
    [datascript :as d]
    [datascript.core :as dc]
    [clojure.walk :as walk]
    [cljs.reader]))

;; Public API

(defn ^:export q [query callback & sources]
  (let [query   (cljs.reader/read-string query)]
    (apply d/q query #(->> (for [tuple %1]
                              (into-array tuple))
                          (into-array)
                          (callback)) sources)))
