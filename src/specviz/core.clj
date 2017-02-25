(ns specviz.core
  "Convert specs into graphviz node maps."
  (:require
    [clojure.java.shell :as sh]
    [clojure.spec :as s]
    [clojure.string :as string]
    [specviz.html :as html]
    [specviz.util :as util]
    [specviz.graphviz
     :as graphviz
     :refer [next-name clean-name render-graphviz h1-color h2-color]]))

;; Utilities which use `specviz.html` to construct table representations of
;; spec data, and render it a an html string in order to be used as the
;; label of a graphviz plaintext node.

(defn- specs->rows
  "Returns a single html row with one cell for each spec."
  [specs row-prefix row-suffix]
  (-> (map util/first* specs)
      (html/row nil)
      (html/decorate-row row-prefix row-suffix)
      vector))

(defn- specs->col
  "Like `specs-row`, but one row for each spec with a single cell."
  [specs row-prefix row-suffix]
  (html/col specs nil))

(defn- table->graphviz-label
  "Generate the (slightly modified) html to be used as a label value for a
  graphviz plaintext node."
  [table]
  (format "<%s>" (html/html table)))

(defn- specs->graphviz-table-node
  "Turn the spec parts into row data for rendering a graphviz table."
  [specs spec-keyword node-name vertical? {:keys [::graphviz/table-opts
                                                  ::graphviz/ellipses-row
                                                  ::graphviz/title-suffix
                                                  ::graphviz/row-prefix
                                                  ::graphviz/row-suffix]}]
  (let [title (str (or spec-keyword " ") " " (or title-suffix " "))

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

;; Other utilities to convert specs into graphviz node maps. These functions
;; could potentially be slightly renamed and moved to `spec.graphviz`.

(defn spec-keyword-graphviz-node
  [spec-keyword]
  (when spec-keyword
    {::graphviz/name (clean-name spec-keyword)
     ::graphviz/label spec-keyword
     ::graphviz/shape "box"
     ::graphviz/style "filled"
     ::graphviz/fillcolor h1-color}))

(defn with-header-graphviz-node
  "If the spec-keyword is not nil, add a graphviz-node indicating the spec's name."
  [header-node-fn spec-keyword nodes]
  (concat
    (when-let [node (header-node-fn spec-keyword)]
      [node
       {::graphviz/from (::graphviz/name node)
        ::graphviz/to (::graphviz/name (first nodes))}])
    nodes))

(def with-name-graphviz-node (partial with-header-graphviz-node spec-keyword-graphviz-node))

;; Core spec -> graphviz conversion code. The `spec->graphviz-nodes*`
;; multimethod is implemented for each of the supported spec expression types.
;; `spec->graphviz-nodes*` returns a collection of  graphviz drawables
;; (nodes & connections). In most cases, it behaves recursively.

(declare spec->graphviz-nodes)

(defmulti spec->graphviz-nodes* (fn [spec-form spec-keyword]
                                  (when (sequential? spec-form)
                                    (first spec-form))))

(s/def ::keys (s/cat
                :keys #{'clojure.spec/keys}
                :parts (s/* (s/cat :type #{:req :opt}
                                   :kws (s/every keyword? :kind vector?)))))

(defmethod spec->graphviz-nodes* 'clojure.spec/keys
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
                     true
                     nil)

        edges (map-indexed
                ;; Similarly, when we introduce h2-colored formatting above,
                ;; we may need to adjust this so that port indexes line up.
                (fn [i kw] (when (util/spec-keyword? kw)
                             (graphviz/cell-edge-to table-node
                                                    (graphviz/port i)
                                                    (clean-name kw))))
                types-and-kws)]
    (conj edges table-node)))

(defn and-graphviz-node
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

(defmethod spec->graphviz-nodes* 'clojure.spec/and
  [spec-form spec-keyword]
  (let [and-node (and-graphviz-node (next-name) (count (rest spec-form)))
        branches (map-indexed
                   (fn [i spec-form]
                     (if (qualified-keyword? spec-form)
                       (graphviz/cell-edge-to
                         and-node (str "f" i) (clean-name spec-form))
                       (let [nodes (spec->graphviz-nodes spec-form)]
                         (conj nodes (graphviz/cell-edge-to
                                       and-node (str "f" i) nodes)))))
                   (rest spec-form))]
    (with-name-graphviz-node spec-keyword (cons and-node branches))))

(defn or-graphviz-node
  "Create a diamond node, used to represent `s/or` specs."
  []
  {::graphviz/name (next-name)
   ::graphviz/label ""
   ::graphviz/shape "diamond"})

(defmethod spec->graphviz-nodes* 'clojure.spec/or
  [spec-form spec-keyword]
  (let [or-node (or-graphviz-node)
        or-pairs (->> spec-form rest (partition 2))
        or-branches (map (fn [[label-kw spec-form]]
                           (if (qualified-keyword? spec-form)
                             (graphviz/cell-edge-to
                               or-node nil (clean-name spec-form) label-kw)
                             (let [nodes (spec->graphviz-nodes spec-form)]
                               (conj nodes (graphviz/cell-edge-to
                                             or-node nil nodes label-kw)))))
                         or-pairs)]
    (with-name-graphviz-node spec-keyword (cons or-node or-branches))))

(defn tabular-spec->graphviz-nodes*
  "Returns a collection of graphviz nodes used to display a tabular entity.

  `spec-parts` are the pieces of a tabular spec. For example, given the spec
               `(s/coll-of int? :foo/bar)` `spec-parts` would have a value of
               `[int? :foo/bar]`.

  `spec-keyword` the registered keyword of the spec, if it has one.

  `table-opts` optionally includes ::graphviz/table-opts,
               ::graphviz/ellipses-row, ::graphviz/title-suffix,
               ::graphviz/row-prefix, ::graphviz/row-suffix"
  [spec-parts spec-keyword & table-opts]
  (let [node-name (or (clean-name spec-keyword) (next-name))
        table-node (specs->graphviz-table-node
                     spec-parts spec-keyword node-name false table-opts)
        nodes (mapcat (fn [spec i]
                        (let [port (graphviz/port i)]
                          (cond
                            ;; For a literal, generate the nodes & connection
                            ;; node.
                            (util/spec-literal? spec)
                            (let [part-nodes (spec->graphviz-nodes spec)]
                              (conj part-nodes
                                    (graphviz/cell-edge-to
                                      table-node port part-nodes)))

                            ;; For an existing spec keyword, only generate the
                            ;; connection node.
                            (util/spec-keyword? spec)
                            [(graphviz/cell-edge-to
                               table-node port (clean-name spec))]

                            ;; Otherwise, if this is a predicate function, or a
                            ;; set for example, don't generate any nodes. We'll
                            ;; display it using the table.
                            )))
                      spec-parts
                      (range))]
    (conj nodes table-node)))

(defmethod spec->graphviz-nodes* 'clojure.spec/every
  [spec-form spec-keyword]
  (if (not (coll? (second spec-form)))
    ;; TODO - factor this!
    (tabular-spec->graphviz-nodes* [(second spec-form)]
                          spec-keyword
                          ::graphviz/table-opts {::cellspacing 6}
                          ::graphviz/ellipses-row true
                          ::graphviz/title-suffix "(...)")
    (tabular-spec->graphviz-nodes* (rest (second spec-form))
                          spec-keyword
                          ::graphviz/table-opts {::cellspacing 6}
                          ::graphviz/ellipses-row true
                          ::graphviz/title-suffix "{...}")))

(defmethod spec->graphviz-nodes* 'clojure.spec/tuple
  [spec-form spec-keyword]
  (tabular-spec->graphviz-nodes* (rest spec-form) spec-keyword
                        ::graphviz/row-suffix " )"
                        ::graphviz/row-prefix "( "))

;; Unsupported

(defmethod spec->graphviz-nodes* 'clojure.spec/multi-spec
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-nodes* 'clojure.spec/fspec
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-nodes* 'clojure.spec/*
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-nodes* 'clojure.spec/&
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-nodes* 'clojure.spec/cat
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-nodes* 'clojure.spec/merge
  [spec-form spec-keyword]
  nil)

(defmethod spec->graphviz-nodes* :default
  [spec-form spec-keyword]
  (with-name-graphviz-node
    spec-keyword
    [{::graphviz/name (next-name)
      ::graphviz/label (print-str spec-form)
      ::graphviz/shape "oval"}]))

(defn spec->graphviz-nodes
  ([spec-form] (spec->graphviz-nodes spec-form nil))
  ([spec-form spec-keyword]
   (spec->graphviz-nodes* spec-form spec-keyword)))

(defn- contains-any?
  [string excludes]
  (some #(.contains string %) excludes))

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
                         (keep
                           (fn [[k v]]
                             (when (= (symbol (namespace k)) root)
                               k))
                           (rest (s/registry))))

        ;; Get nested specs for all of the starting specs.
        nested-specs (distinct (mapcat util/nested-specs starting-specs))

        ;; Filter the excluded specs.
        filtered-specs (remove (fn [spec] (contains-any?
                                            (namespace spec)
                                            excluded-namespaces))
                               nested-specs)

        ;; Generate the graphviz dot document.
        dot (->> filtered-specs
                 (map (fn [spec-kw] (spec->graphviz-nodes (s/form (s/get-spec spec-kw)) spec-kw)))
                 flatten
                 (remove nil?)
                 (map (partial util/conform-or-throw ::graphviz/drawable))
                 (map render-graphviz)
                 (apply str))]

    (graphviz/generate-image! dot filename)))

(comment
  (diagram :specviz.graphviz/drawable nil "foo")
  (diagram 'specviz.example nil "foo"))
