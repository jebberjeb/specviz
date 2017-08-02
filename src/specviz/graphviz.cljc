(ns specviz.graphviz
  "Tools to work with graphviz data.

  Graphviz data consists of two primary elements, nodes and connections. A
  graphviz document consists of a sequence of these elements in which order
  can be important.

  This namespace contains the following:

    - specs for graphviz data
    - functions to convert graphviz data into a graphviz dot string
    - a function to render a graphviz dot string into a png image
  "
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [specviz.spec :as spec]
    [specviz.util :as util]
    [viz.core :as viz]))

;; *** Graphviz specs ***

(s/def ::shape #{"record" "box" "oval" "plaintext" "circle" "diamond"
                 "trapezium" "square" "folder" "doublecircle" "point"})
(s/def ::connection (s/keys :req [::to ::from]
                            :opt [::label ::line-style ::constraint
                                  ::line-direction]))
(s/def ::node (s/keys :req [::name]
                      :opt [::label ::shape ::fillcolor ::style
                            ::height ::width]))
(s/def ::drawable (s/or :connection ::connection
                        :node ::node))
(s/def ::line-style #{:dotted :solid})
(s/def ::line-direction #{:none :both :forward :back})

; *** graphviz data -> dot ***

;; `render-graphviz` generates the graphviz dot string for a graphviz element
(defmulti render-graphviz first)

(defmulti render-graphviz-node ::shape)

#?(:cljs
   (defn format
     [format-str & args]
     (reduce (fn [string arg]
               (string/replace-first string "%s" arg))
             format-str
             args)))

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
  (render-graphviz-node* (update node ::label #(format "\"%s\""
                                                       (or % (::name node))))))

(defmethod render-graphviz :node
  [[_ node]]
  (render-graphviz-node node))

(defmethod render-graphviz :connection
  [[_ {:keys [::to ::from ::line-style ::constraint ::line-direction ::label]}]]
  (format "%s->%s [label=\"%s\",style=%s,constraint=%s,dir=%s];\n"
          from
          to
          (or label "")
          (when line-style (name line-style))
          (if (nil? constraint) true constraint)
          (when line-direction (name line-direction))))

(defn dot-string
  "Generate the graphviz dot string for a sequence of graphviz element
  (connection & node) maps."
  [elements]
  (->> elements
       (map (partial spec/conform-or-throw ::drawable))
       (map render-graphviz)
       (apply str)))

;; *** graphviz utility funcitons ***

(let [id (atom 0)]
  (defn- next-id
    []
    (swap! id inc)
    @id))

(defn next-name
  "Returns a unique name for use with a graphviz node."
  []
  (str "node" (next-id)))

(defn clean-name
  "Turn the qualified keyword into a graphviz friendly name"
  [qkw]
  (when qkw
    (-> (apply str (namespace qkw) (name qkw))
        (string/replace ">" "")
        (string/replace "." "")
        (string/replace ":" "")
        (string/replace "-" "")
        (string/replace "?" ""))))

(defn get-root-name
  "Gets the name of the tree's root graphviz node."
  [nodes]
  (::name (util/first* nodes)))

(defn connect
  "Make a connection from one node to another node.

  `from` the origin node (map)

  `from-port` (optional) if `from-node` is a table, the id (string) of a port
              (cell) of the node, from which the connection should originate

  `to` the destination of the connection, can be a node (map), sequence of
       nodes, or the name of a node (string)

  `label` the connection's label"
  [& {:keys [from from-port to label]}]
  (assert (and from to))
  (let [from-str (if from-port
                   (str (::name from) ":" from-port)
                   (::name from))]
    {::from from-str
     ::label label
     ::to (cond (string? to) to
                (coll? to) (get-root-name to))}))

;; *** dot executable wrapper functions ***

(defn generate-image!
  "Generates two files (1) <filename>.dot containing the dot string, and
  <filename>.png containing the graphviz rendering as a png file, using the
  `dot` executable binary."
  [dot-string filename]
  (let [dot-string' (str "digraph {\nrankdir=LR;\n" dot-string "\n}")]

    #?(:clj
       (do
         (spit (str filename ".dot") (util/add-line-no dot-string'))
         (spit (str filename ".svg")
               (viz/image (string/replace dot-string' "\n" "")))))

    #?(:cljs
       (viz/image (string/replace dot-string' "\n" "")))))

(comment
  (let [data [{::name "foo"
               ::label "Foo"
               ::shape "box"}
              {::name "bar"
               ::label "Bar"
               ::shape "diamond"}
              {::from "foo"
               ::to "bar"
               ::label "baz"}]]
    (generate-image! (dot-string data) "baz")))
