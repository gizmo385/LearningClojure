(ns graphs.graph)

(definterface IVertex
  (addEdge [key])
  (removeEdge [key])
  (degree []))

(deftype Vertex [key value edges]
  Object
  (equals [self other]
    "Two vertices are equal if their keys are equal"
    (= (.key self) (.key other)))
  (hashCode [self]
    "The hash for a vertex is the hash of its key"
    (hash key))

  IVertex
  (degree [self]
    "Returns the number of edges incident on this vertex"
    (count (.edges self)))

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

(definterface IGraph
  (addVertex [key value])
  (removeVertex [key])
  (addEdge [u-key v-key])
  (removeEdge [u-key v-key])
  (find [key])
  (order [])
  (size []))

(deftype Graph [vertices]
  IGraph
  (order [self]
    "Returns the number of vertices in this graph"
    (count (.vertices self)))

  (size [self]
    "Returns the number of edges in the graph"
    (/ (reduce + 0 (map #(.degree %1) vertices)) 2))

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
        (let [vertices (set (map #(.removeEdge %1 key) vertices))]
          (Graph. (disj vertices vertex))))))

  (addEdge [self u-key v-key]
    "Adds an edge between the vertices specified by the provided keys"
    (let [u (.find self u-key)
          v (.find self v-key)]
      (if (not-any? nil? '(u v))
        (let [new-u (.addEdge u v-key)
              new-v (.addEdge v u-key)]
          (Graph. (conj (disj vertices u v) new-u new-v))))))

  (removeEdge [self u-key v-key]
    (let [u (.find self u-key)
          v (.find self v-key)]
      (if (not-any? nil? '(u v))
          (let [new-u (.removeEdge u v-key)
                new-v (.removeEdge v u-key)]
            (Graph. (conj (disj vertices u v) new-u new-v))))))
  (find [self key]
    "Finds the key in the graph. Returns nil if it is not present"
    (loop [curr (first vertices) vertices (rest (.vertices self))]
      (cond
        (nil? curr) nil
        (= (.key curr) key) curr
        :else (recur (first vertices) (rest vertices))))))

(definterface INetwork
  (weightOfEdge [u-key, v-key]))

(deftype Network
  [graph              ; The graph that maintains the vertex set
   weight-function]   ; A function that maps a pair of vertices to some meaningful value
  IGraph
  (addVertex [self key value]
    "Add a vertex to a graph. Throws an IllegalArgumentException if the graph already contains a
     vertex with the provided key"
    (Network. (.addVertex (.graph self) key value) weight-function))

  (removeVertex [self key]
    "Attempts to remove the vertex specified with the provided key. Throws an
     IllegalArgumentException if that key is not present in this graph"
    (Network. (.removeVertex (.graph self) key) weight-function))

  (addEdge [self u-key v-key]
    "Adds an edge between the vertices specified by the provided keys"
    (Network (.addEdge (.graph self) u-key v-key) weight-function))

  (removeEdge [self u-key v-key]
    (Network (.removeEdge (.graph self) u-key v-key) weight-function))

  (find [self key]
    "Finds the key in the graph. Returns nil if it is not present"
    (.find (.graph self) key))

  (order [self]
    "Returns the number of vertices in this graph"
    (.order (.graph self)))

  (size [self]
    "Returns the number of edges in the graph"
    (.size (.graph self)))
  INetwork
  (weightOfEdge [self u-key v-key]
    (let [u (.find self u-key)
          v (.find self v-key)]
      (weight-function u v))))

(defn eulerian? [^IGraph g]
  "A graph is Eulerian if and only if every vertex is even"
  (and (map even? (map #(.degree %1) (.vertices g)))))

(defn traversible? [^IGraph g]
  "A graph is traversible if and only if it contains exactly two odd vertices"
  (= (count (filter odd? (map #(.degree %1) (.vertices g)))) 2))

(defn createGraph
  "Creates a graph with either no vertices or the specified vertices"
  ([] (createGraph ()))
  ([vertices] (Graph. (set vertices)))
  ([key value] (.addVertex (createGraph) key value))
  ([key value & pairs]
   (if (even? (count pairs))
     (loop [g (createGraph key value) pairs pairs]
       (if (empty? pairs)
         g
         (recur (.addVertex g (first pairs) (second pairs)) (rest (rest pairs)))))
     (throw (IllegalArgumentException. "There must be an even number of arguments!")))))

(defn createNetwork
  "Creates a network, which is defined as a graph and a weight function"
  ([weight-function] (Network. (createGraph) weight-function))
  ([weight-function vertices] (Network. (createGraph vertices) weight-function))
  ([weight-function key value] (Network. (createGraph key value) weight-function))
  ([weight-function key value & pairs]
   (Network. (createGraph key value pairs) weight-function)))
