(ns specviz.spec
  "Analyze clojure.spec specs."
  (:require
    [clojure.spec.alpha :as s]))

(defn registered?
  "Returns true if `x` is the keyword of a registered spec?"
  [x]
  (some? (s/get-spec x)))

(defn depends-on*
  [names spec-form]
  (cond (coll? spec-form)
        (doseq [s spec-form] (depends-on* names s))

        (and (registered? spec-form)
             (not (contains? @names spec-form)))
        (do (swap! names conj spec-form)
            (depends-on* names (s/form (s/get-spec spec-form))))))

(defn depends-on
  "Returns a collection of the qualified-keywords of all specs referenced
  by the spec-form, transatively."
  [spec-name]
  (let [names (atom #{spec-name})]
    (depends-on* names (s/form (s/get-spec spec-name)))
    @names))

(defn conform-or-throw
  [spec x]
  "Return the result of conforming `x` using `spec`. If `x` does not conform,
  throw an exception."
  (when-let [reason (s/explain-data spec x)]
    (throw (ex-info "invalid spec" {:reason reason})))
  (s/conform spec x))

(defn literal?
  "Returns true if `x` is a spec literal, ex: `(clojure.spec/coll-of int?)`."
  [x]
  (when (coll? x)
    (= (namespace (first x))
       "clojure.spec")))
