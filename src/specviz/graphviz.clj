(ns specviz.graphviz
  "Graphviz stuff."
  (:require
    [clojure.java.shell :as sh]
    [clojure.spec :as s]
    [clojure.string :as string]
    [specviz.util :as util]))

(def h1-color "#CCCCCC")
(def h2-color "#EEEEEE")

(s/def ::shape #{"record" "box" "oval" "plaintext" "circle" "diamond"})
(s/def ::connection (s/keys :req [::to ::from]
                            :opt [::label ::line-style ::constraint
                                  ::line-direction]))
(s/def ::node (s/keys :req [::name ::label]
                      :opt [::shape ::fillcolor ::style
                            ::height ::width]))
(s/def ::drawable (s/or :connection ::connection
                        :node ::node))

(s/def ::line-style #{:dotted :solid})
(s/def ::line-direction #{:none :both :forward :back})

(let [id (atom 0)]
  (defn next-id
    []
    (swap! id inc)
    @id))

(defn next-name
  []
  (format "node%s" (next-id)))

(defn wrap-quotes
  [s]
  (format "\"%s\"" s))

(defn clean-name
  "Turn the spec name (a namespaced keyword) into a GraphViz friendly name"
  [spec-name]
  (when spec-name
    (-> (apply str (namespace spec-name) (name spec-name))
        (string/replace ">" "")
        (string/replace "." "")
        (string/replace ":" "")
        (string/replace "-" "")
        (string/replace "?" ""))))

(defmulti render-graphviz-node ::shape)

(defn render-graphviz-node*
  [{:keys [::name ::shape ::label ::style ::fillcolor ::height ::width]}]
  (format "%s [shape=%s,label=%s,style=%s,fillcolor=\"%s\", height=%s,width=%s
          ];\n"
          name
          shape
          label
          style
          fillcolor
          height
          width))

(defmethod render-graphviz-node "plaintext"
  [node]
  (render-graphviz-node* node))

(defmethod render-graphviz-node :default
  [node]
  (render-graphviz-node* (update node ::label wrap-quotes)))

(defmulti render-graphviz first)

(defmethod render-graphviz :node
  [[_ node]]
  (render-graphviz-node node))

(defmethod render-graphviz :connection
  [[_ {:keys [::to ::from ::line-style ::constraint ::line-direction ::label]}]]
  (format "%s->%s [label=\"%s\"style=%s,constraint=%s,dir=%s];\n"
          from
          to
          (or label "")
          (when line-style (name line-style))
          (if (nil? constraint) true constraint)
          (when line-direction (name line-direction))))

(defn port
  "Returns a string representing the 'port' id, for a cell in a graphviz table."
  [index]
  (format "f%s" index))

(defn get-root-name
  "Gets the name of the tree's root graphviz-node."
  [nodes]
  (::name (util/first* nodes)))

(defn cell-edge-to
  "Make an edge from a cell to something."
  ([from-node from-port to]
   (cell-edge-to from-node from-port to ""))
  ([from-node from-port to label]
   (let [from-node-name (::name from-node)
         from (if from-port
                (str from-node-name ":" from-port)
                from-node-name)]
     {::from from
      ::label label
      ::to (cond (string? to) to
                 (coll? to) (get-root-name to))})))

;; dot wrapper functions.

(defn generate-image!
  [dot-string filename]
  (spit (str filename ".dot") (util/add-line-no dot-string))
  (sh/sh "dot" "-Tpng" (str "-o" filename ".png")
         :in
         (str "digraph {\nrankdir=LR;\n" dot-string "\n}")))
