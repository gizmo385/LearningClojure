(ns graphs.graph2)

(deftype Vertex
  [key        ; The unique key that identifies this vertex
   value]     ; The value that is contained within this vertex
  Object
  (equals [self other]
    "Two vertices are equal if their keys are equal"
    (= (.key self) (.key other)))

  (hashCode [self]
    "The hash code of a vertex is the hash of its key"
    (hash (.key self))))

(defrecord Weighted-Edge
  [key      ; The key for the vertex that this edge is directed towards
   weight]) ; The weight of an edge is some real number

(defrecord Graph
  [vertices]) ; A set of vertices that are contained in this graph

; Vertex mutltimethods
(defmulti degree
  "The degree of a vertex is the number of edges incident to it"
  (fn [u] [(type u)]))

(defmethod degree Vertex [u]
  (-> u .edges count))

(defmethod degree :default [u]
  (throw (IllegalArgumentException. (str "Cannot find the degree of a" (type u)))))

; Graph multimethods
