(defproject specviz "0.2.5-SNAPSHOT"
  :description "Generate Graphviz images from Clojure specs"
  :url "https://github.com/jebberjeb/specviz"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}

  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn])
