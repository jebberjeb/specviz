(ns specviz.html
  "Functions to work with hiccup data."
  (:require
    [clojure.string :as string]
    #?(:clj [hiccup.core :as html]
       :cljs [crate.core :as html])))

(def h1-color "#CCCCCC")
(def h2-color "#EEEEEE")

(defn port
  "Returns a string representing the 'port' id, for a cell in a graphviz table."
  [index]
  (str "f" index))

(defn decorate-row
  "Adds the prefix to the value of the row's first cell, and the suffix to
  the value of the row's last cell."
  [row prefix suffix]
  (-> row
      (update-in [1 2] #(str prefix %))
      (update-in [(dec (count row)) 2] #(str % suffix))))

(defn header-row
  "Returns a header row with the `title`."
  [title rows]
  [:tr [:td {:colspan (dec (count (first rows)))
             :bgcolor h1-color} title]])

(defn ellipses-row
  "Returns a row with the same number of cells as the rows in `rows`, where
  each cell's value is '...'"
  [rows]
  `[:tr ~@(repeat (dec (count (first rows))) [:td "..."])])

(defn escape
  [s]
  (-> s
      (string/replace ">" "&gt;")))

(defn row
  "Returns a single row with one cell for each spec. One small detail of
  graphviz has leaked through here -- the port attribute, which is used
  for connectiions.

  `cell-attributes` a map of the properties to be used by each cell
                    ex `{:bgcolor \"#cccccc\" :height 10 :width 10}`
  "
  [values cell-attributes]
  `[:tr ~@(map-indexed
            (fn [i v] [:td
                       (merge {:port (port i)}
                              cell-attributes)
                       (escape (str v))])
            values)])

(defn col
  "Like `row`, but creates a row for each value with one cell."
  [values cell-attributes]
  (vec (map-indexed
         (fn [i v] [:tr [:td
                         (merge {:port (port i)} cell-attributes)
                         (escape (str v))]])
         values)))

(defn table
  "Returns a table with the `rows` and `table-opts`."
  [title rows table-opts]
  `[:table
    {:border ~(or (:border table-opts) 0)
     :cellspacing ~(or (:cellspacing table-opts) 0)
     :cellpadding 3
     :cellborder ~(or (:cellborder table-opts) 1)}
    ~(when title (header-row title rows))
    ~@rows])

(defn html
  "Generate the html string for the table data."
  [table]
  (html/html table))
