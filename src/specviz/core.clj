(ns specviz.core
  "Generate diagrams from specs."
  (:require
    [clojure.java.shell :as sh]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [specviz.html :as html]
    [specviz.graphviz
     :as graphviz
     :refer [next-name clean-name render-graphviz h1-color h2-color]]
    [specviz.spec :as spec]
    [specviz.util :as util]))

;; *** Table Nodes ***
;; Use `specviz.html` to construct table representations of spec data, and
;; render it a an html string in order to be used as the label of a graphviz
;; plaintext node.

(defn- specs->rows
  "Returns a single html row with one cell for each spec."
  [specs row-prefix row-suffix]
  (-> (map (comp util/strip-core
                 util/first*) specs)
      (html/row nil)
      (html/decorate-row row-prefix row-suffix)
      vector))

(defn- specs->col
  "Like `specs-row`, but one row for each spec with a single cell."
  [specs row-prefix row-suffix]
  (html/col (map util/strip-core specs) nil))

(defn- table->graphviz-label
  "Generate the (slightly modified) html to be used as a label value for a
  graphviz plaintext node."
  [table]
  (format "<%s>" (html/html table)))

(defn- specs->graphviz-table-node
  "Turn the spec parts into row data for rendering a graphviz table."
  [specs spec-keyword node-name {:keys [table-opts ellipses-row title-suffix
                                        row-prefix row-suffix vertical?
                                        hide-title?]}]
  (let [title (when-not hide-title?
                (str (or spec-keyword " ") " " (or title-suffix " ")))

        ;; Create html rows from the specs, and format them.
        rows ((if vertical? specs->col specs->rows)
              specs row-prefix row-suffix)

        ;; Create an html table for the row, and potentially ellipses row.
        table (html/table
                title
                (concat rows (when ellipses-row [(html/ellipses-row rows)]))
                table-opts)]

    ;; Create a graphviz plaintext node who's label is the html string
    ;; generated using the table created above.
    {::graphviz/name node-name
     ::graphviz/shape "plaintext"
     ::graphviz/label (table->graphviz-label table)}))

;; *** spec -> graphviz ***
;; Core spec -> graphviz conversion code.

 (defn- with-name-graphviz-node
  "If the spec-keyword is not nil, add a graphviz-node indicating the spec's name."
  [spec-keyword nodes]
  (concat
    (when spec-keyword
      (let [node {::graphviz/name (clean-name spec-keyword)
                  ::graphviz/label spec-keyword
                  ::graphviz/shape "box"
                  ::graphviz/style "filled"
                  ::graphviz/fillcolor h1-color}]
        [node
         {::graphviz/from (::graphviz/name node)
          ::graphviz/to (::graphviz/name (first nodes))}]))
    nodes))


(declare spec->graphviz-elements)

;; The `spec->graphviz-elements*` multimethod is implemented for each of the
;; supported spec expression types.  `spec->graphviz-elements*` returns a
;; collection of  graphviz drawables (nodes & connections). In most cases, it
;; behaves recursively.

(defmulti spec->graphviz-elements* (fn [spec-form spec-keyword]
                                  (when (sequential? spec-form)
                                    (first spec-form))))

(s/def ::keys (s/cat
                :keys #{'clojure.spec/keys}
                :parts (s/* (s/cat :type #{:req :opt :req-un :opt-un}
                                   :kws (s/every keyword? :kind vector?)))))

(defmethod spec->graphviz-elements* 'clojure.spec/keys
  [spec-form spec-keyword]
  (let [node-name (or (clean-name spec-keyword) (next-name))
        parts (:parts (s/conform ::keys spec-form))

        types-and-kws (mapcat (fn [{:keys [:type :kws]}]
                                (cons type kws))
                              parts)

        table-node (specs->graphviz-table-node
                     ;; Combine the :type & :kws attributes for now, into one
                     ;; list.  In the future, we'll need h2-colored
                     ;; ::graphviz/bgcolor for the type rows.
                     types-and-kws
                     spec-keyword
                     node-name
                     {:vertical? true})

        edges (map-indexed
                ;; Similarly, when we introduce h2-colored formatting above,
                ;; we may need to adjust this so that port indexes line up.
                (fn [i kw] (when (spec/registered? kw)
                             (graphviz/connect :from table-node
                                               :from-port (graphviz/port i)
                                               :to (clean-name kw))))
                types-and-kws)]
    (conj edges table-node)))

(defn- and-graphviz-node
  "Create the vertical 'fork' node, used to represent `s/and` specs."
  [node-name fork-count]
  (let [height (/ 150 fork-count)]
    {::graphviz/name node-name
     ::graphviz/width 0.25 ;; HACK, reduce the outer shape's width to fit.
     ::graphviz/label (table->graphviz-label
                        (html/table
                          nil
                          (html/col (repeat fork-count "")
                                    {:height height
                                     :width 12
                                     :bgcolor "#666666"})
                          nil))
     ::graphviz/shape "plaintext"}))

(defmethod spec->graphviz-elements* 'clojure.spec/and
  [spec-form spec-keyword]
  (let [and-node (and-graphviz-node (next-name) (count (rest spec-form)))
        branches (map-indexed
                   (fn [i spec-form]
                     (if (qualified-keyword? spec-form)
                       (graphviz/connect :from and-node
                                         :from-port (str "f" i)
                                         :to (clean-name spec-form))
                       (let [nodes (spec->graphviz-elements spec-form)]
                         (conj nodes (graphviz/connect :from and-node
                                                       :from-port (str "f" i)
                                                       :to nodes)))))
                   (rest spec-form))]
    (with-name-graphviz-node spec-keyword (cons and-node branches))))

(defn- or-graphviz-node
  "Create a diamond node, used to represent `s/or` specs."
  []
  {::graphviz/name (next-name)
   ::graphviz/label ""
   ::graphviz/shape "diamond"})

(defmethod spec->graphviz-elements* 'clojure.spec/or
  [spec-form spec-keyword]
  (let [or-node (or-graphviz-node)
        or-pairs (->> spec-form rest (partition 2))
        or-branches (map (fn [[label-kw spec-form]]
                           (if (qualified-keyword? spec-form)
                             (graphviz/connect :from or-node
                                               :to (clean-name spec-form)
                                               :label label-kw)
                             (let [nodes (spec->graphviz-elements spec-form)]
                               (conj nodes (graphviz/connect :from or-node
                                                             :to nodes
                                                             :label label-kw)))))
                         or-pairs)]
    (with-name-graphviz-node spec-keyword (cons or-node or-branches))))

(defn- tabular-spec->graphviz-elements*
  "Returns a collection of graphviz nodes used to display a tabular entity.

  `spec-parts` are the pieces of a tabular spec. For example, given the spec
               `(s/coll-of int? :foo/bar)` `spec-parts` would have a value of
               `[int? :foo/bar]`.

  `spec-keyword` the registered keyword of the spec, if it has one.

  `recursive?` if true, generate the nodes for the spec recursively.

  `table-opts` optionally includes :table-opts, :ellipses-row, :title-suffix,
               :row-prefix, :row-suffix :vertica? :hide-title?"
  [spec-parts spec-keyword recursive? & table-opts]
  (let [node-name (or (clean-name spec-keyword) (next-name))
        table-node (specs->graphviz-table-node
                     spec-parts spec-keyword node-name table-opts)
        nodes (when recursive?
                (mapcat (fn [spec i]
                        (let [port (graphviz/port i)]
                          (cond
                            ;; For a literal, generate the nodes & connection
                            ;; node.
                            (spec/literal? spec)
                            (let [part-nodes (spec->graphviz-elements spec)]
                              (conj part-nodes
                                    (graphviz/connect :from table-node
                                                      :from-port port
                                                      :to part-nodes)))

                            ;; For an existing spec keyword, only generate the
                            ;; connection node.
                            (spec/registered? spec)
                            [(graphviz/connect :from table-node
                                               :from-port port
                                               :to (clean-name spec))]

                            ;; Otherwise, if this is a predicate function, or a
                            ;; set for example, don't generate any nodes. We'll
                            ;; display it using the table.
                            )))
                      spec-parts
                      (range)))]
    (conj nodes table-node)))

(defmethod spec->graphviz-elements* 'clojure.spec/every
  [spec-form spec-keyword]
  (if (not (coll? (second spec-form)))
    ;; TODO - factor this!
    (tabular-spec->graphviz-elements* [(second spec-form)]
                          spec-keyword
                          true
                          :table-opts {:cellspacing 0}
                          :ellipses-row true
                          :title-suffix "(...)")
    (tabular-spec->graphviz-elements* (rest (second spec-form))
                          spec-keyword
                          true
                          :table-opts {:cellspacing 0}
                          :ellipses-row true
                          :title-suffix "{...}")))

(defmethod spec->graphviz-elements* 'clojure.spec/tuple
  [spec-form spec-keyword]
  (tabular-spec->graphviz-elements* (rest spec-form)
                                    spec-keyword
                                    true
                                    :row-suffix " )"
                                    :row-prefix "( "))

(defmethod spec->graphviz-elements* 'clojure.spec/nilable
  [spec-form spec-keyword]
  (spec->graphviz-elements `(s/or :nil nil?
                                  :not-nil ~@(rest spec-form))
                           spec-keyword))

;; Unsupported

(defmethod spec->graphviz-elements* 'clojure.spec/multi-spec
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-elements* 'clojure.spec/fspec
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-elements* 'clojure.spec/*
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-elements* 'clojure.spec/&
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-elements* 'clojure.spec/cat
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-elements* 'clojure.spec/merge
  [spec-form spec-keyword]
  nil)

(defn other-spec->graphviz-elements
  [spec-form spec-keyword]
  (with-name-graphviz-node
    spec-keyword
    [{::graphviz/name (next-name)
      ::graphviz/label (-> spec-form
                           pr-str
                           util/strip-core)
      ::graphviz/shape "oval"}]))

(defn coll-spec->graphviz-elements
  [first-row spec-form spec-keyword]
  (with-name-graphviz-node
    spec-keyword
    (tabular-spec->graphviz-elements*
      (cons first-row (map pr-str spec-form))
      nil
      false
      :table-opts {:cellspacing 0
                   :cellborder 0
                   :border 1}
      :vertical? true
      :hide-title? true)))

(defmethod spec->graphviz-elements* :default
  [spec-form spec-keyword]
  ((cond (vector? spec-form)
         (partial coll-spec->graphviz-elements "[...]")
         (set? spec-form)
         (partial coll-spec->graphviz-elements "#{...}")
         (map? spec-form)
         (partial coll-spec->graphviz-elements "{...}")
         :else
         other-spec->graphviz-elements) spec-form spec-keyword))

(defn- spec->graphviz-elements
  "Return a sequence of graphviz elements used to render the spec."
  ([spec-form] (spec->graphviz-elements spec-form nil))
  ([spec-form spec-keyword]
   (spec->graphviz-elements* spec-form spec-keyword)))

;; *** Diagram ***

(defn diagram
  "Generate a diagram of the specs.

  `filename` only the name of the file. Both <filename>.png, and <filename>.dot
  files will be generated.

  `root` can be a keyword, naming a spec in the registry, or a namespace symbol
  from which to load all specs. From these starting points, all
  dependent specs are included, recursively.
  ex: `:specviz.graphviz/shape`, `'specviz.graphviz`

  `excluded-namespaces` collection of strings representing namespaces, or
  partial namespaces, which should be excluded from the
  diagram. ex: `[\"clojure.core\" \"string\"]`"
  [root excluded-namespaces filename]

  ;; Make sure the specs are loaded (into the registry).
  (require (if (qualified-keyword? root)
             (symbol (namespace root))
             root))

  (let [;; Start with either the specified root spec, or all of the specs in
        ;; the root namespace.
        starting-specs (if (qualified-keyword? root)
                         [root]
                         (->> (s/registry)
                              (map key)
                              (filter #(= root (symbol (namespace %))))))

        ;; Get nested specs for all of the starting specs.
        nested-specs (distinct (mapcat spec/depends-on starting-specs))

        ;; Filter the excluded specs.
        filtered-specs (remove (fn [spec] (some
                                            #(.contains (namespace spec) %)
                                            (conj excluded-namespaces
                                                  "clojure.core")))
                               nested-specs)

        ;; Generate the graphviz dot string.
        dot (->> filtered-specs
                 (map (fn [spec-kw] (spec->graphviz-elements
                                      (s/form (s/get-spec spec-kw)) spec-kw)))
                 flatten
                 (remove nil?)
                 graphviz/dot-string)]

    (graphviz/generate-image! dot filename)))

(comment
  (specviz.core/diagram :specviz.graphviz/drawable nil "bar")
  (specviz.core/diagram 'specviz.example nil "foo1"))
