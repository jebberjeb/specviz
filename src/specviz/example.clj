(ns specviz.example
  "A place for example specs."
  (:require
    [clojure.spec :as s]))

(s/def ::ident keyword?)
(s/def ::eid int?)
(s/def ::lookup-ref (s/tuple keyword? any?))
(s/def ::identifier
  (s/or :ident ::ident
        :eid ::eid
        :lookup-ref ::lookup-ref))

(s/def ::shape #{::square ::circle ::triangle})

(s/def ::test-or
  (s/or :foo keyword?
        :bar ::eid
        :baz (s/or :pos pos?
                   :neg neg?
                   :zero zero?)
        :qux (s/tuple keyword? string?)
        :qul (s/keys :req [::shape ::foo])))

(s/def ::test-and
  (s/and ::shape
         string?))

(s/def ::tuple-w-nested-spec
    (s/tuple keyword?
             (s/or :pos pos?
                   :even even?)
             ::ident))

(s/def ::test-map-of
  (s/map-of ::shape ::test-or))

(s/def ::test-coll-of
  (s/coll-of ::shape))
