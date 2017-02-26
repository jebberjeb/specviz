(ns specviz.util
  "General utilities."
  (:require
    [clojure.spec :as s]
    [clojure.string :as string]))

(defn add-line-no
  "Add line numbers to strings, for debugging."
  [s]
  (->> s
       string/split-lines
       (map-indexed (fn [idx line] (str idx ": " line)))
       (string/join "\n")))

(def concatv (comp vec concat))

(defn first*
  "Returns the first item in x, if x is sequential, else x."
  [x]
  (if (sequential? x) (first x) x))
