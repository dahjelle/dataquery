(defproject dataquery "0.1.0"
  :description "An implementation of the datalog query language with user-provided indexes."
  :license {:name "Eclipse"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/dahjelle/dataquery"

  :dependencies [
    [org.clojure/clojure "1.6.0"]
    [org.clojure/clojurescript "0.0-2341"]
  ]
  :cljsbuild {
    :builds [
      { :id "release"
        :source-paths ["src"]
        :assert false
        :compiler {
          :output-to     "web/datascript.min.js"
          :optimizations :advanced
          :pretty-print  false
          :preamble      ["datascript/preamble.js"]
          :elide-asserts true
        }}
  ]}

  :profiles {
    :dev {
      :plugins [
        [lein-cljsbuild "1.0.3"]
        [com.cemerick/clojurescript.test "0.3.1"]
      ]
      :cljsbuild {
        :builds [
          { :id "dev"
            :source-paths ["src" "test"]
            :compiler {
              :output-to     "web/datascript.js"
              :output-dir    "web/out"
              :optimizations :none
              :source-map    true
            }}
          { :id "testable"
            :source-paths ["src" "test"]
            :compiler {
              :output-to     "web/datascript.testable.js"
              :optimizations :simple ; needs to be :simple or :advanced for Node tests
            }}
        ]
        :test-commands {
          ;"test.datascript"    [ "phantomjs" :runner "web/datascript.testable.js" ]
          "test.datascript"    [ "node" :node-runner "web/datascript.testable.js" ]
          ;"test.datascript.js" [ "node" "test/js/index.js" ] ; requires a specific CouchDB database to pass
        }
      }
    }
  })
