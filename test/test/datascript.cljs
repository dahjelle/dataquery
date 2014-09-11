(ns test.datascript
  (:require-macros
    [cemerick.cljs.test :refer (is deftest with-test run-tests testing test-var)])
  (:require
    [datascript.core :as dc]
    [datascript :as d]
    [cljs.reader]
    [cemerick.cljs.test :as t]))

(enable-console-print!)

(deftest test-joins
  (let [db (-> (dc/init-db)
               (dc/add-record 1 :name "Ivan")
               (dc/add-record 2 :name "Petr")
               (dc/add-record 3 :name "Ivan")
               (dc/add-record 1 :age 15)
               (dc/add-record 2 :age 37)
               (dc/add-record 3 :age 37)
               (dc/add-record 4 :age 15))]
    (d/q '[:find ?w
          :where [?w :name]] (fn [result]
                                (println "result" result)
                                (is (= result #{[1] [2] [3]}))) db)))


;(deftest test-joins
;  (let [db (-> (d/empty-db)
;               (d/db-with [ { :db/id 1, :name  "Ivan", :age   15 }
;                            { :db/id 2, :name  "Petr", :age   37 }
;                            { :db/id 3, :name  "Ivan", :age   37 }
;                            { :db/id 4, :age 15 }]))]
;    (is (= (d/q '[:find ?e
;                  :where [?e :name]] db)
;           #{[1] [2] [3]}))
;    (is (= (d/q '[:find  ?e ?v
;                  :where [?e :name "Ivan"]
;                         [?e :age ?v]] db)
;           #{[1 15] [3 37]}))
;    (is (= (d/q '[:find  ?e1 ?e2
;                  :where [?e1 :name ?n]
;                         [?e2 :name ?n]] db)
;           #{[1 1] [2 2] [3 3] [1 3] [3 1]}))
;    (is (= (d/q '[:find  ?e ?e2 ?n
;                  :where [?e :name "Ivan"]
;                         [?e :age ?a]
;                         [?e2 :age ?a]
;                         [?e2 :name ?n]] db)
;           #{[1 1 "Ivan"]
;             [3 3 "Ivan"]
;             [3 2 "Petr"]}))))
;
;
;(deftest test-q-many
;  (let [db (-> (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
;               (d/db-with [ [:db/add 1 :name "Ivan"]
;                            [:db/add 1 :aka  "ivolga"]
;                            [:db/add 1 :aka  "pi"]
;                            [:db/add 2 :name "Petr"]
;                            [:db/add 2 :aka  "porosenok"]
;                            [:db/add 2 :aka  "pi"] ]))]
;    (is (= (d/q '[:find  ?n1 ?n2
;                  :where [?e1 :aka ?x]
;                         [?e2 :aka ?x]
;                         [?e1 :name ?n1]
;                         [?e2 :name ?n2]] db)
;           #{["Ivan" "Ivan"]
;             ["Petr" "Petr"]
;             ["Ivan" "Petr"]
;             ["Petr" "Ivan"]}))))
;
;
;(deftest test-q-coll
;  (let [db [ [1 :name "Ivan"]
;             [1 :age  19]
;             [1 :aka  "dragon_killer_94"]
;             [1 :aka  "-=autobot=-"] ] ]
;    (is (= (d/q '[ :find  ?n ?a
;                   :where [?e :aka "dragon_killer_94"]
;                          [?e :name ?n]
;                          [?e :age  ?a]] db)
;           #{["Ivan" 19]})))
;
;  (testing "Query over long tuples"
;    (let [db [ [1 :name "Ivan" 945 :db/add]
;               [1 :age  39     999 :db/retract]] ]
;      (is (= (d/q '[ :find  ?e ?v
;                     :where [?e :name ?v]] db)
;             #{[1 "Ivan"]}))
;      (is (= (d/q '[ :find  ?e ?a ?v ?t
;                     :where [?e ?a ?v ?t :db/retract]] db)
;             #{[1 :age 39 999]})))))
;
;
;(deftest test-q-in
;  (let [db (-> (d/empty-db)
;               (d/db-with [ { :db/id 1, :name  "Ivan", :age   15 }
;                            { :db/id 2, :name  "Petr", :age   37 }
;                            { :db/id 3, :name  "Ivan", :age   37 }]))
;        query '{:find  [?e]
;                :in    [$ ?attr ?value]
;                :where [[?e ?attr ?value]]}]
;    (is (= (d/q query db :name "Ivan")
;           #{[1] [3]}))
;    (is (= (d/q query db :age 37)
;           #{[2] [3]}))
;
;    (testing "Named DB"
;      (is (= (d/q '[:find  ?a ?v
;                    :in    $db ?e
;                    :where [$db ?e ?a ?v]] db 1)
;             #{[:name "Ivan"]
;               [:age 15]})))
;
;    (testing "DB join with collection"
;      (is (= (d/q '[:find  ?e ?email
;                    :in    $ $b
;                    :where [?e :name ?n]
;                           [$b ?n ?email]]
;                  db
;                  [["Ivan" "ivan@mail.ru"]
;                   ["Petr" "petr@gmail.com"]])
;             #{[1 "ivan@mail.ru"]
;               [2 "petr@gmail.com"]
;               [3 "ivan@mail.ru"]})))
;
;    (testing "Relation binding"
;      (is (= (d/q '[:find  ?e ?email
;                    :in    $ [[?n ?email]]
;                    :where [?e :name ?n]]
;                  db
;                  [["Ivan" "ivan@mail.ru"]
;                   ["Petr" "petr@gmail.com"]])
;             #{[1 "ivan@mail.ru"]
;               [2 "petr@gmail.com"]
;               [3 "ivan@mail.ru"]})))
;
;    (testing "Tuple binding"
;      (is (= (d/q '[:find  ?e
;                    :in    $ [?name ?age]
;                    :where [?e :name ?name]
;                           [?e :age ?age]]
;                  db ["Ivan" 37])
;             #{[3]})))
;
;    (testing "Collection binding"
;      (is (= (d/q '[:find  ?attr ?value
;                    :in    $ ?e [?attr ...]
;                    :where [?e ?attr ?value]]
;                  db 1 [:name :age])
;             #{[:name "Ivan"] [:age 15]}))))
;
;  (testing "Query without DB"
;    (is (= (d/q '[:find ?a ?b
;                  :in   ?a ?b]
;                10 20)
;           #{[10 20]}))))
;
;
;(deftest test-nested-bindings
;  (is (= (d/q '[:find  ?k ?v
;                :in    [[?k ?v] ...]
;                :where [(> ?v 1)]]
;              {:a 1, :b 2, :c 3})
;         #{[:b 2] [:c 3]}))
;
;  (is (= (d/q '[:find  ?k ?min ?max
;                :in    [[?k ?v] ...] ?minmax
;                :where [(?minmax ?v) [?min ?max]]
;                       [(> ?max ?min)]]
;              {:a [1 2 3 4]
;               :b [5 6 7]
;               :c [3]}
;              #(vector (reduce min %) (reduce max %)))
;         #{[:a 1 4] [:b 5 7]}))
;
;  (is (= (d/q '[:find  ?k ?x
;                :in    [[?k [?min ?max]] ...] ?range
;                :where [(?range ?min ?max) [?x ...]]
;                       [(even? ?x)]]
;              {:a [1 7]
;               :b [2 4]}
;              range)
;         #{[:a 2] [:a 4] [:a 6]
;           [:b 2]})))
;
;
;(deftest test-user-funs
;  (let [db (-> (d/empty-db {:parent {:parent {:db/valueType :db.valueType/ref}}})
;               (d/db-with [ { :db/id 1, :name  "Ivan",  :age   15 }
;                            { :db/id 2, :name  "Petr",  :age   22, :height 240 :parent 1}
;                            { :db/id 3, :name  "Slava", :age   37 :parent 2}]))]
;    (testing "get-else"
;      (is (= (d/q '[:find ?e ?age ?height
;                    :in $
;                    :where [?e :age ?age]
;                           [(get-else $ ?e :height 300) ?height]] db)
;             #{[1 15 300] [2 22 240] [3 37 300]})))
;
;    (testing "get-some"
;      (is (= (d/q '[:find ?e ?v
;                    :in $
;                    :where [?e :name ?name]
;                           [(get-some $ ?e :height :age) ?v]] db)
;             #{[1 15] [2 240] [3 37]})))
;
;    (testing "missing?"
;      (is (= (d/q '[:find ?e ?age
;                    :in $
;                    :where [?e :age ?age]
;                           [(missing? $ ?e :height)]] db)
;             #{[1 15] [3 37]})))
;
;    (testing "missing? back-ref"
;      (is (= (d/q '[:find ?e
;                    :in $
;                    :where [?e :age ?age]
;                    [(missing? $ ?e :_parent)]] db)
;             #{[3]})))
;
;    (testing "Built-in predicate"
;      (is (= (d/q '[:find  ?e1 ?e2
;                    :where [?e1 :age ?a1]
;                           [?e2 :age ?a2]
;                           [(< ?a1 18 ?a2)]] db)
;             #{[1 2] [1 3]})))
;
;    (testing "Passing predicate as source"
;      (is (= (d/q '[:find  ?e
;                    :in    $ ?adult
;                    :where [?e :age ?a]
;                           [(?adult ?a)]]
;                  db
;                  #(> % 18))
;             #{[2] [3]})))
;
;    (testing "Calling a function"
;      (is (= (d/q '[:find  ?e1 ?e2 ?e3
;                    :where [?e1 :age ?a1]
;                           [?e2 :age ?a2]
;                           [?e3 :age ?a3]
;                           [(+ ?a1 ?a2) ?a12]
;                           [(= ?a12 ?a3)]]
;                  db)
;             #{[1 2 3] [2 1 3]})))))
;
;
;(deftest test-rules
;  (let [db [                  [5 :follow 3]
;            [1 :follow 2] [2 :follow 3] [3 :follow 4] [4 :follow 6]
;                          [2         :follow           4]]]
;    (is (= (d/q '[:find  ?e1 ?e2
;                  :in    $ %
;                  :where (follow ?e1 ?e2)]
;                db
;               '[[(follow ?x ?y)
;                  [?x :follow ?y]]])
;           #{[1 2] [2 3] [3 4] [2 4] [5 3] [4 6]}))
;
;    (testing "Rule with branches"
;      (is (= (d/q '[:find  ?e2
;                    :in    $ ?e1 %
;                    :where (follow ?e1 ?e2)]
;                  db
;                  1
;                 '[[(follow ?e2 ?e1)
;                    [?e2 :follow ?e1]]
;                   [(follow ?e2 ?e1)
;                    [?e2 :follow ?t]
;                    [?t  :follow ?e1]]])
;             #{[2] [3] [4]})))
;
;    (testing "Recursive rules"
;      (is (= (d/q '[:find  ?e2
;                    :in    $ ?e1 %
;                    :where (follow ?e1 ?e2)]
;                  db
;                  1
;                 '[[(follow ?e1 ?e2)
;                    [?e1 :follow ?e2]]
;                   [(follow ?e1 ?e2)
;                    [?e1 :follow ?t]
;                    (follow ?t ?e2)]])
;             #{[2] [3] [4] [6]}))
;
;      (is (= (d/q '[:find ?e1 ?e2
;                     :in $ %
;                     :where (follow ?e1 ?e2)]
;                    [[1 :follow 2] [2 :follow 3]]
;                   '[[(follow ?e1 ?e2)
;                      [?e1 :follow ?e2]]
;                     [(follow ?e1 ?e2)
;                      (follow ?e2 ?e1)]])
;           #{[1 2] [2 3] [2 1] [3 2]}))
;
;      (is (= (d/q '[:find ?e1 ?e2
;                     :in $ %
;                     :where (follow ?e1 ?e2)]
;                    [[1 :follow 2] [2 :follow 3] [3 :follow 1]]
;                   '[[(follow ?e1 ?e2)
;                      [?e1 :follow ?e2]]
;                     [(follow ?e1 ?e2)
;                      (follow ?e2 ?e1)]])
;           #{[1 2] [2 3] [3 1] [2 1] [3 2] [1 3]})))
;
;    (testing "Mutually recursive rules"
;      (is (= (d/q '[:find  ?e1 ?e2
;                    :in    $ %
;                    :where (f1 ?e1 ?e2)]
;                  [[0 :f1 1]
;                   [1 :f2 2]
;                   [2 :f1 3]
;                   [3 :f2 4]
;                   [4 :f1 5]
;                   [5 :f2 6]]
;                 '[[(f1 ?e1 ?e2)
;                    [?e1 :f1 ?e2]]
;                   [(f1 ?e1 ?e2)
;                    [?t :f1 ?e2]
;                    (f2 ?e1 ?t)]
;                   [(f2 ?e1 ?e2)
;                    [?e1 :f2 ?e2]]
;                   [(f2 ?e1 ?e2)
;                    [?t :f2 ?e2]
;                    (f1 ?e1 ?t)]])
;            #{[0 1] [0 3] [0 5]
;              [1 3] [1 5]
;              [2 3] [2 5]
;              [3 5]
;              [4 5]}))))
;
;  (testing "Specifying db to rule"
;    (is (= (d/q '[ :find ?n
;                    :in   $sexes $ages %
;                    :where ($sexes male ?n)
;                           ($ages adult ?n) ]
;                  [["Ivan" :male] ["Darya" :female] ["Oleg" :male] ["Igor" :male]]
;                  [["Ivan" 15] ["Oleg" 66] ["Darya" 32]]
;                  '[[(male ?x)
;                     [?x :male]]
;                    [(adult ?y)
;                     [?y ?a]
;                     [(>= ?a 18)]]])
;           #{["Oleg"]}))))
;
;(deftest test-aggregates
;  (let [monsters [ ["Cerberus" 3]
;                   ["Medusa" 1]
;                   ["Cyclops" 1]
;                   ["Chimera" 1] ]]
;    (testing "with"
;      (is (= (d/q '[ :find ?heads
;                     :with ?monster
;                     :in   [[?monster ?heads]] ]
;                  [ ["Medusa" 1]
;                    ["Cyclops" 1]
;                    ["Chimera" 1] ])
;             [[1] [1] [1]])))
;
;    (testing "Wrong grouping without :with"
;      (is (= (d/q '[ :find (sum ?heads)
;                     :in   [[?monster ?heads]] ]
;                  monsters)
;             [[4]])))
;
;    (testing "Multiple aggregates, correct grouping with :with"
;      (is (= (d/q '[ :find (sum ?heads) (min ?heads) (max ?heads) (count ?heads)
;                     :with ?monster
;                     :in   [[?monster ?heads]] ]
;                  monsters)
;             [[6 1 3 4]])))
;
;    (testing "Grouping and parameter passing"
;      (is (= (set (d/q '[ :find ?color (max ?amount ?x) (min ?amount ?x)
;                          :in   [[?color ?x]] ?amount ]
;                       [[:red 1]  [:red 2] [:red 3] [:red 4] [:red 5]
;                        [:blue 7] [:blue 8]]
;                       3))
;             #{[:red  [3 4 5] [1 2 3]]
;               [:blue [7 8]   [7 8]]})))
;
;    (testing "Custom aggregates"
;      (is (= (set (d/q '[ :find ?color (?agg ?x)
;                          :in   [[?color ?x]] ?agg ]
;                       [[:red 1]  [:red 2] [:red 3] [:red 4] [:red 5]
;                        [:blue 7] [:blue 8]]
;                       #(reverse (sort %))))
;             #{[:red [5 4 3 2 1]] [:blue [8 7]]})))))