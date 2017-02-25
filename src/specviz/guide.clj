(ns specviz.guide
  "Slurp spec code from the spec guide, load it in this ns"
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string]
    [specviz.util :as util]))

(def guide-url "https://raw.githubusercontent.com/clojure/clojure-site/master/content/guides/spec.adoc")

;; Use of non-greedy *? here to avoid SOE.
(def code (->> (re-seq #"(?:clojure\]\n----\n)((?:.|\s)*?)(?:\s----\s)"
                       (slurp guide-url))
               (map second)
               (mapcat string/split-lines)
               (remove #(#{\: \= \[} (first %)))
               (string/join "\n")))

(load-string code)
