(ns gizmo385.graph)

(definterface IGraph
  (addVertex [key value])
  (removeVertex [key])
  (addEdge [u-key v-key])
  (removeEdge [u-key v-key])
  (find [key]))

(definterface IVertex
  (addEdge [key])
  (removeEdge [key])
  (hasEdge? [key]))

(deftype Vertex [key value edges]
  Object
  (equals [self other]
    "Two vertices are equal if their keys are equal"
    (= (.key self) (.key other)))
  (hashCode [self]
    "The hash for a vertex is the hash of its key"
    (hash key))

  IVertex
  (addEdge [self key]
    "Adds an edge to this vertex.  This edge cannot be a self-loop."
    (if (= (.key self) key)
      (throw (IllegalArgumentException. "A vertex cannot have self loops!"))
      (Vertex. (.key self) value (conj edges key))))
  (removeEdge [self key]
    "Returns the vertex that does not contain an edge to the vertex with the specified key"
    (Vertex. (.key self) value (disj edges key))))

(defmethod print-method Vertex [v ^java.io.Writer w]
  (.write w (str "(" (.key v) ", " (.value v) ")")))

(defrecord Graph [vertices]
  IGraph
  (addVertex [self key value]
    "Add a vertex to a graph. Throws an IllegalArgumentException if the graph already contains a
     vertex with the provided key"
    (if (nil? (.find self key))
      (Graph. (conj vertices (Vertex. key value (set ()))))
      (throw (IllegalArgumentException. "That key already exists in this graph!"))))

  (removeVertex [self key]
    "Attempts to remove the vertex specified with the provided key. Throws an
     IllegalArgumentException if that key is not present in this graph"
    (let [vertex (.find self key)]
      (if (nil? vertex)
        (throw (IllegalArgumentException. "That key does not exist!"))
        (Graph. (disj vertices vertex)))))

  (addEdge [self u-key v-key]
    "Adds an edge between the vertices specified by the provided keys"
    (let [u (.find self u-key)
          v (.find self v-key)
          new-u (.addEdge u v-key)
          new-v (.addEdge v u-key)]
      (Graph. (conj (disj vertices u v) new-u new-v))))
  (removeEdge [self u-key v-key])
  (find [self key]
    "Finds the key in the graph. Returns nil if it is not present"
    (loop [curr (first vertices) vertices (rest (.vertices self))]
      (cond
        (nil? curr) nil
        (= (.key curr) key) curr
        :else (recur (first vertices) (rest vertices))))))

(defn createGraph
  "Creates a graph with either no vertices or the specified vertices"
  ([] (createGraph ()))
  ([vertices] (Graph. (set vertices))))

(def graph (createGraph))
(def graph (.addVertex graph "Hello" "World"))
(println (.vertices graph))
