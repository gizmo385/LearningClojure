(ns graphs.graph2
  (:use [clojure.set :only [union difference]]))

; Utility functions
(defn- not-nil? [x]
  "Returns true if x is not nil"
  (not (nil? x)))

(defn members? [xs coll]
  "Returns true if every element in xs is a member of coll"
  (every? not-nil? (for [x xs] (some #{x} coll))))

; Definition for a vertex in a graph
(deftype Vertex [key value]
  Object
  (equals [self other]
    "Two vertices are equal if their keys are equal"
    (= (.key self) (.key other)))

  (hashCode [self]
    "The hash for a vertex is the hash of its key"
    (hash key)))

; Graph types
(defrecord DiGraph [vertices edges])
(defrecord SimpleGraph [vertices edges])
(defrecord Network [vertices edges weight-function])

; Establish graph hierarchy
(map #(derive %1 ::graph) [Network DiGraph SimpleGraph])

; Edge types
(defrecord Edge [u v])
(defrecord DirectedEdge [to from])
(defrecord WeightedEdge [u v weight])

; Establish edge hierarchy
(map #(derive %1 ::edge) [Edge DirectedEdge WeightedEdge])

; Type constructors
(defn new-vertex
  "A vertex is a (key, value) pair and a set of edges (which can be empty)"
  ([key value] (Vertex. key value)))

(defn new-simple-graph
  "Creates a new simple graph, which consists of a vertex set and a set of edges between vertices"
  ([] (new-simple-graph #{} #{}))
  ([vertices] (new-simple-graph vertices #{}))
  ([vertices edges]
   (let [vertices*  (if (set? vertices) vertices (set vertices))
         edges*     (if (set? edges) edges (set edges))]
     (SimpleGraph. vertices* edges*))))

(defn new-digraph
  "Creates a new directed graph, which consists of a vertex set and a set of edges which explicitly
   travel from one vertex to another."
  ([] (new-digraph #{} #{}))
  ([vertices] (new-digraph vertices #{}))
  ([vertices edges]
   (let [vertices*  (if (set? vertices) vertices (set vertices))
         edges*     (if (set? edges) edges (set edges))]
     (DiGraph. vertices edges))))

(defn new-network
  "Creates a new directed graph, which consists of a vertex set and a set of edges which explicitly
   travel from one vertex to another.

   The weight function should be a function that takes two vertices and returns an integer"
  ([weight-function] (new-network #{} #{} weight-function))
  ([vertices weight-function] (new-network vertices #{} weight-function))
  ([vertices edges weight-function]
   (let [vertices*  (if (set? vertices) vertices (set vertices))
         edges*     (if (set? edges) edges (set edges))]
     (Network. vertices edges weight-function))))

; Graph protocol
(defprotocol Graph
  "A protocol that defines the functions a graph must implement"
  (degree         [graph vertex])
  (degrees        [graph])
  (order          [graph])
  (size           [graph])
  (find-vertex    [graph key])
  (add-vertices   [graph & pairs])
  (remove-vertex  [graph u])
  (add-edges      [graph & pairs])
  (remove-edge    [graph u v]))

; Type checking functions
(defn graph? [g]
  "Returns true if g is a graph type"
  (satisfies? Graph g))

(defn edge? [e]
  "Returns true if e is an edge"
  (isa? e ::edge))

(defn vertex? [v]
  "Returns true if v is a vertex"
  (instance? Vertex v))

; Graph invariant functions
(defn eulerian? [g]
  "Returns true if g is a Eulerian graph"
  (if (graph? g)
    (every? even? (degrees g))
    false))

(defn traversible? [g]
  "Returns true if g is a traversible graph"
  (if (graph? g)
    (= 2 (count (filter odd? (degrees g))))
    false))

(defn complete? [g]
  "Returns true if g is a complete graph"
  (if (graph? g)
    (let [required-degree (dec (size g))]
      (every? #(= required-degree %) (degrees g)))
    false))

; Functions which share a common implementation amongst different graph types
(defn- add-vertices* [graph pairs]
  "Creates vertices based upon the (key, value) pairs and adds them to the graph"
  (if (even? (count pairs))
    (let [vertices (for [[key value] (partition 2 pairs)]
                     (new-vertex key value))]
      (assoc graph :vertices (union (graph :vertices) (set vertices))))
    (throw (IllegalArgumentException. "Must have an even number of pairs!"))))

(defn- find-vertex* [graph key]
  "Finds the vertex in the graph with the specified vertex"
  (loop [vertices (graph :vertices)]
    (cond
      (empty? vertices) nil
      (= ((first vertices) :key) key) (first vertices)
      :else (recur (rest vertices)))))

(defn- add-edges* [graph edge-creation-fn pairs]
  "Adds edges, created using a function that takes two vertices, between vertices supplied as pairs"
  (if (and (even? (count pairs)) (every? (vertex? pairs)))
    (let [pairs*    (partition 2 pairs)
          new-edges (set (for [[u v] pairs* :when (not= u v)] (apply edge-creation-fn u v)))]
      (assoc graph :edges (union (graph :edges) new-edges)))
    (throw (IllegalArgumentException. "Vertices must be in pairs!"))))

; Graph implementations
(extend-protocol Graph
  SimpleGraph
    (degree [graph vertex]
      "The degree of a vertex in a simple graph is the number of edges adjacent to it."
      (count (filter (fn [edge] (contains? [(:u edge) (:v edge)] vertex)) (graph :edges))))

    (degrees [graph]
      "Returns a list of the degrees in the graph"
      (list (for [vertex (graph :vertices)] (degree graph vertex))))

    (order [graph]
      "The order of a graph is the number of vertices it has"
      (count (graph :vertices)))

    (size [graph]
      "The size of a graph is the number of edges it has"
      (count (graph :edges)))

    (add-vertices [graph & pairs]
      "Creates vertices based upon the (key, value) pairs and adds them to the graph"
      (add-vertices* graph pairs))

    (find-vertex [graph key]
      "Finds the vertex in the graph with the specified vertex"
      (find-vertex* graph key))

    (remove-vertex [graph u]
      (let [cut-edges (filter #(members? [u] [(:u %) (:v %)]) (graph :edges))]
        (assoc graph  :vertices (disj (graph :vertices) u)
                      :edges    (difference (graph :edges) cut-edges))))

    (add-edges [graph & pairs]
      "Adds an edge to the graph for each pair."
      (add-edges* graph (fn [u v] (Edge. u v)) pairs))

    (remove-edge [graph u v]
      (let [cut-edges (filter #(members? [u v] [(:u %) (:v %)]) (graph :edges))]
        (assoc graph :edges (difference (graph :edges) cut-edges))))

  DiGraph
    (degree [graph vertex]
      "The degree of a vertex in a digraph is the number of edges directed TOWARDS it."
      (count (filter (fn [edge] (= vertex (:to edge)) (graph :edges)))))

    (degrees [graph]
      "Returns a list of the degrees in the graph"
      (list (for [vertex (graph :vertices)] (degree graph vertex))))

    (order [graph]
      "The order of a graph is the number of vertices it has"
      (count (graph :vertices)))

    (size [graph]
      "The size of a graph is the number of edges it has"
      (count (graph :edges)))

    (find-vertex [graph key]
      "Finds the vertex in the graph with the specified vertex"
      (find-vertex* graph key))

    (add-vertices [graph & pairs]
      "Creates vertices based upon the (key, value) pairs and adds them to the graph"
      (add-vertices* graph pairs))

    (remove-vertex [graph u]
      (let [cut-edges (filter #(members? [u] [(:to %) (:from %)]) (graph :edges))]
        (assoc graph  :vertices (disj (graph :vertices) u)
                      :edges    (difference (graph :edges) cut-edges))))

    (add-edges [graph & pairs]
      "Adds an edge to the graph for each pair. These edges are directed such that the first vertex has an edge
       directed towards the second edge. Calls to this function should take the form:

          (add-edges graph a b b c c d u v)

       Which will result in the following edges: (a-->b), (b-->c), (c-->d), and (u-->v).
      "
      (add-edges* graph (fn [source destination] (DirectedEdge. source destination)) pairs))

    (remove-edge [graph source destination]
      "Removes any edges that travels from the source to the destination"
      (let [cut-edges (filter #(and (= source (% :from)) (= destination (% :to))) (graph :edges))]
        (assoc graph :edges (difference (graph :edges) cut-edges))))

  Network
    (degree [graph vertex]
      "The degree of a vertex in a simple graph is the number of edges adjacent to it."
      (count (filter (fn [edge] (contains? [(:u edge) (:v edge)] vertex)) (graph :edges))))

    (degrees [graph]
      "Returns a list of the degrees in the graph"
      (list (for [vertex (graph :vertices)] (degree graph vertex))))

    (order [graph]
      "The order of a graph is the number of vertices it has"
      (count (graph :vertices)))

    (size [graph]
      "The size of a graph is the number of edges it has"
      (count (graph :edges)))

    (find-vertex [graph key]
      "Finds the vertex in the graph with the specified vertex"
      (find-vertex* graph key))

    (add-vertices [graph & pairs]
      "Creates vertices based upon the (key, value) pairs and adds them to the graph"
      (add-vertices* graph pairs))

    (remove-vertex [graph u]
      (let [cut-edges (filter #(members? [u] [(:u %) (:v %)]) (graph :edges))]
        (assoc graph  :vertices (disj (graph :vertices) u)
                      :edges    (difference (graph :edges) cut-edges))))

    (add-edges [graph & pairs]
      "Adds an edge to the graph for each pair."
      (let [f (graph :weight-function)]
        (add-edges* graph (fn [u v] (WeightedEdge. u v (apply f u v))) pairs)))

    (remove-edge [graph u v]
      "Removes the edges which travel between vertices u and v"
      (let [cut-edges (filter #(members? [u v] [(:u %) (:v %)]) (graph :edges))]
        (assoc graph :edges (difference (graph :edges) cut-edges))))

) ; extend-protocol
