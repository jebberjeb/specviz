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

(defn spec-keyword?
  "Is `x` the keyword of a registered spec?"
  [x]
  (s/get-spec x))

(defn walk-spec
  "Returns a collection of the qualified-keywords of all specs referenced
  by the spec-form, transatively."
  [names spec-form]
  (cond (coll? spec-form)
        (doseq [s spec-form] (walk-spec names s))

        (spec-keyword? spec-form)
        (do (swap! names conj spec-form)
            (walk-spec names (s/form (s/get-spec spec-form))))))

(defn nested-specs
  "Returns a collection of the names of all specs referred to by a root spec"
  [spec-name]
  (let [names (atom [spec-name])]
    (walk-spec names (s/form (s/get-spec spec-name)))
    @names))

(defn conform-or-throw
  [spec x]
  "If a spec fails to conform, throw an exception."
  (when-let [reason (s/explain-data spec x)]
    (throw (ex-info "invalid spec" {:reason reason})))
  (s/conform spec x))

(defn spec-literal?
  "Returns true if `x` is a spec literal, ex: `(clojure.spec/coll-of int?)`."
  [x]
  (when (coll? x)
    (= (namespace (first x))
       "clojure.spec"))) 
