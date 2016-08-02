(ns specviz.core
  (:require [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.java.shell :as sh]))

(s/def ::connection (s/keys :req [::to ::from]))
(s/def ::node (s/keys :req [::name ::label]
                      :opt [::placeholder-for ::shape ::fillcolor ::style]))
(s/def ::drawable (s/or :connection ::connection
                        :node ::node))

(s/def ::ident keyword?)
(s/def ::eid int?)
(s/def ::lookup-ref (s/tuple keyword? any?))
(s/def ::identifier
  (s/or :ident ::ident
        :eid ::eid
        :lookup-ref ::lookup-ref))

(s/def ::test-or
  (s/or :foo keyword?
        :bar ::eid
        :baz (s/or :pos pos?
                   :neg neg?
                   :zero zero?)))

(s/def ::tuple-w-nested-spec
    (s/tuple keyword?
             (s/or :pos pos?
                   :even even?)
             ::ident))

(let [id (atom 0)]
  (defn next-id
    []
    (swap! id inc)
    @id))

(defn wrap-quotes
  [s]
  (format "\"%s\"" s))

(defn clean-name
  "Turn the spec name (a namespaced keyword) into a GraphViz friendly name"
  [spec-name]
  (when spec-name
    (-> (apply str (namespace spec-name) (name spec-name))
        (string/replace "." "")
        (string/replace ":" "")
        (string/replace "-" ""))))

(declare spec->graph)

(defn namespaced-keyword?
  [x]
  (and (keyword? x) (namespace x)))

(defmulti render-graphviz first)

(defmethod render-graphviz :node
  [[_ {:keys [::name ::shape ::label ::style ::fillcolor]}]]
  (format "%s [shape=%s,label=\"%s\",style=%s,fillcolor=\"%s\"];\n"
          name
          shape
          label
          style
          fillcolor))

(defmethod render-graphviz :connection
  [[_ {:keys [::to ::from ::style ::constraint]}]]
  (format "%s->%s [style=%s,constraint=%s];\n"
          from
          to
          style
          (if (nil? constraint) true constraint)))

(defn keys-spec->graph
    [spec-form spec-name]
    ;; subgraph here? w/ connected boxes?
    ;; Or a record laid out like a table?
    )

;; Utilities

(defn spec-name-node
  ([spec-name] (spec-name-node spec-name false))
  ([spec-name placeholder]
   (let [cleaned-name (clean-name spec-name)]
     (cond-> {::name (if (or (nil? spec-name) placeholder)
                       (format "node%s" (next-id))
                       cleaned-name)
              ::label spec-name
              ::shape "box"
              ::style "filled"
              ::fillcolor (if-not placeholder "#CCCCCC" "#FFFFFF")}
       placeholder
       (assoc ::placeholder-for cleaned-name)))))

(defn get-root-name
  "Gets the name of the tree's root node."
  [nodes]
  (if (sequential? nodes)
    (::name (first nodes))
    (::name nodes)))

(defn connections-of-sequence
    [nodes]
    (->> nodes
         (partition 2 1)
         (map (fn [[from to]] {::to (get-root-name to)
                               ::from (get-root-name from)
                               ::constraint false}))))

(defn fan-out-connections
  [from-node to-nodes connection-style]
  (map (fn [node]
         {::from (get-root-name from-node)
          ::to (get-root-name node)
          ::style connection-style})
       to-nodes))

;; TODO - tighten this up and reuse it for all of the tx-fn
#_(s/fdef or-spec->graph
        :args (s/cat :spec-form any? :spec-name any?)
        :ret (s/coll-of ::drawable))

;; TODO - refactor, too big!
(defn or-spec->graph
  [spec-form spec-name]
  (let [;node-name (or (clean-name spec-name) (format "node%s" (next-id)))
        group-id (next-id)
        or-pairs (->> spec-form rest (partition 2))]
    ;; TODO - I think this can become a conj
    (concat
      (when spec-name
        (let [name-node (spec-name-node spec-name)]
          [name-node
           {::from (::name name-node)
            ::to (str "or" group-id)}]))
      [;; Or node
       {::name (str "or" group-id)
        ::label "or"
        ::shape "circle"}]
      ;; Pred nodes
      (mapcat (fn [[label-kw spec-form]]
                (let [pred-node-name (str (clean-name label-kw) group-id)]
                  [{::name pred-node-name
                    ::label label-kw
                    ::shape "box"
                    ::style "rounded"}
                   {::from (str "or" group-id)
                    ::to pred-node-name}
                   (if (namespaced-keyword? spec-form)
                     {::from pred-node-name
                      ::to (clean-name spec-form)}
                     (let [nodes (spec->graph spec-form)]
                       [nodes
                        {::from pred-node-name
                         ::to (::name (first nodes))}]))]))
              or-pairs))))

(defn tuple-spec->graph
  [spec-form spec-name]
  (let [head-node {::name (str "tuple" (next-id))
                   ::label "(...)"
                   ::shape "circle"}
        element-nodes (map (fn [spec]
                             (if (namespaced-keyword? spec)
                               (spec-name-node spec true)
                               (spec->graph spec))) (rest spec-form))
        nodes (concat
                [(when spec-name
                   (let [name-node (spec-name-node spec-name)]
                     [name-node
                      {::from (::name name-node)
                       ::to (::name head-node)}]))
                 head-node]
                element-nodes)]
    (concat nodes
            (fan-out-connections head-node element-nodes "dotted")
            (connections-of-sequence element-nodes))))

(defn primitive-spec->graph
  [spec-form spec-name]
  (if-let [node-name (clean-name spec-name)]
    [{::name node-name
      ::label (format "{%s|%s}" spec-name spec-form)
      ::shape "record"}]
    [{::name (format "node%s" (next-id))
      ::label spec-form
      ::shape "oval"}]))

(defn spec->graph
  ([spec-form] (spec->graph spec-form nil))
  ([spec-form spec-name]
   (println spec-form)
   (cond
     (coll? spec-form)
     (case (first spec-form)
       clojure.spec/tuple
       (tuple-spec->graph spec-form spec-name)
       clojure.spec/or
       (or-spec->graph spec-form spec-name)
       clojure.spec/keys
       (keys-spec->graph spec-form spec-name)
       clojure.spec/fspec
       nil)
     :else
     (primitive-spec->graph spec-form spec-name))))

(defn add-placeholder-edges
  [nodes]
  (concat nodes
          (->> nodes
               (filter ::placeholder-for)
               (remove nil?)
               (map (fn [node]
                      {::from (::name node)
                       ::to (::placeholder-for node)
                       ::style "dotted"})))))

(defn generate-image!
  [dot-string]
  (println dot-string)
  (sh/sh "dot" "-Tpng" "-ofoo.png"
         :in
         (str "digraph {\nrankdir=LR;\n" dot-string "\n}")))

;; Test code
(generate-image! (->> (rest (s/registry))
                      #_(filter (fn [[k v]] (= k ::connection)))
                      (map (fn [[k v]] (spec->graph (s/form v) k)))
                      flatten
                      (remove nil?)
                      add-placeholder-edges
                      (map (partial s/conform ::drawable))
                      (map render-graphviz)
                      (apply str)
                      #_(str "\n subgraph cluster_1 { tuple11->node13\n
                          tuple11->or14\n
                           tuple11->node17\n
                           node13->or14->node17 }")))

#_(.printStackTrace *e *out*)
