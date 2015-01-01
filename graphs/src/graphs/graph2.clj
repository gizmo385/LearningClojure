(ns graphs.graph2
  (:use [clojure.set :only [union difference]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous utility functions used throughout the file
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- not-nil? [x]
  "Returns true if x is not nil"
  (not (nil? x)))

(defn members? [xs coll]
  "Returns true if every element in xs is a member of coll"
  (every? not-nil? (for [x xs] (some #{x} coll))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Defining vertices
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype Vertex [key value]
  Object
  (equals [self other]
    "Two vertices are equal if their keys are equal"
    (= (.key self) (.key other)))

  (hashCode [self]
    "The hash for a vertex is the hash of its key"
    (hash key)))

(defn vertex? [v]
  "Returns true if v is a vertex"
  (instance? Vertex v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Defining graphs and graph hierarchy
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defrecord DiGraph [vertices edges])
(defrecord SimpleGraph [vertices edges])
(defrecord Network [vertices edges weight-function])

; Establish graph hierarchy
(map #(derive %1 ::graph) [Network DiGraph SimpleGraph])

(defn graph? [g]
  "Returns true if g is a graph type"
  (satisfies? Graph g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Defining edges and edge hierarchy
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Edge         [u v])
(defrecord DirectedEdge [to from])
(defrecord WeightedEdge [u v weight])

; Establish edge hierarchy
(map #(derive %1 ::edge) [Edge DirectedEdge WeightedEdge])

(defn edge? [e]
  "Returns true if e is an edge"
  (isa? e ::edge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions which can be used to create new graphs and graph elements
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions which establish an invariant on the graph
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions which share a common implementation amongst different graph types
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Implementation of the graph protocol for Simple Graphs, Networks, and DiGraphs
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Implementation Split:
;
; The implementation for graph functions below uses type-based dispatching with multimethods isntead
; of protocols.
;
; This implementation relies on a macro supplied below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- emit-method [name dispatch-value args implementation]
  "Outputs a method definition"
  (if (symbol? name)
    `(defmethod ~name ~dispatch-value ~args ~implementation)
    `(defmethod ~(symbol name) ~dispatch-value ~args ~implementation)))

(defmacro defmethods [name dispatch-values args implementation]
  "Defines a method for each dispatch value that uses the provided implementation"
  (cons `do (for [value dispatch-values] (emit-method name value `[~@args] implementation))))

; Graph multimethods
(defmulti degree
  "The degree of a vertex in the graph is the number of edges traveling to it."
  (fn [graph vertex] (type graph)))

(defmulti degrees
  "Returns a list of the degrees of the vertices in the graph."
  (fn [graph] (type graph)))

(defmulti order
  "The order of a graph is the number of vertices it has."
  (fn [graph] (type graph)))

(defmulti size
  "The size of a graph is the number of edges it has."
  (fn [graph] (type graph)))

(defmulti find-vertex
  "Returns the vertex in the graph that has a particular key."
  (fn [graph key] (type graph)))

(defmulti add-vertices
  "Adds the vertices specified as key value argument pairs. For example: (add-vertices graph :a :b)
   adds a vertex with key :a and value :b."
  (fn [graph & pairs] (type graph)))

(defmulti remove-vertex
  "Removes the vertex u, along with any edges incident on it, from the graph"
  (fn [graph u] (type graph)))

(defmulti add-edges
  "Adds edges between vertices specified as argument pairs."
  (fn [graph & pairs] (type graph)))

(defmulti remove-edge
  "Removes any edge existing between vertices u and v"
  (fn [graph u v] (type graph)))

; Calculating the degree of a vertex
(defmethods degree (SimpleGraph Network) [graph vertex]
  (count (filter (fn [edge] (contains? [(:u edge) (:v edge)] vertex)) (graph :edges))))

(defmethod degree DiGraph [graph vertex]
  (count (filter (fn [edge] (= vertex (:to edge)) (graph :edges)))))

; Retrieving a list of the degrees of all vertices
(defmethods degrees (SimpleGraph Network DiGraph) [graph vertex]
  (list (for [vertex (graph :vertices)] (degree graph vertex))))

; Determining the order of a graph
(defmethods order (SimpleGraph Network DiGraph) [graph]
  (count (graph :vertices)))

; Determining the size of a graph
(defmethods size (SimpleGraph Network DiGraph) [graph]
  (count (graph :edges)))

; Locating a vertex in a graph
(defmethods find-vertex (SimpleGraph Network DiGraph) [graph key]
  (loop [vertices (graph :vertices)]
    (cond
      (empty? vertices) nil
      (= ((first vertices) :key) key) (first vertices)
      :else (recur (rest vertices)))))

; Adding vertices to a graph
(defmethods add-vertices (SimpleGraph Network DiGraph) [graph & pairs]
  (if (even? (count pairs))
    (let [vertices (for [[key value] (partition 2 pairs)] (new-vertex key value))]
      (assoc graph :vertices (union (graph :vertices) (set vertices))))
    (throw (IllegalArgumentException. "Must have an even number of pairs!"))))

; Removing an edge from the graph
(defmethods remove-edge (SimpleGraph Network) [graph u v]
  (let [cut-edges (filter #(members? [u v] [(:u %) (:v %)]) (graph :edges))]
    (assoc graph :edges (difference (graph :edges) cut-edges))))

(defmethod remove-edge DiGraph [graph source destination]
  (let [cut-edges (filter #(and (= source (% :from)) (= destination (% :to))) (graph :edges))]
    (assoc graph :edges (difference (graph :edges) cut-edges))))

; Adding edges
(defmethod add-edges SimpleGraph [graph & pairs]
  (add-edges* graph (fn [u v] (Edge. u v)) pairs))

(defmethod add-edges Network [graph & pairs]
  (let [f (graph :weight-function)]
    (add-edges* graph (fn [u v] (WeightedEdge. u v (apply f u v))) pairs)))

(defmethod add-edges DiGraph [graph & pairs]
  (add-edges* graph (fn [u v] (DirectedEdge. u v)) pairs))

; Removing vertices
(defmethods remove-vertex (SimpleGraph Network) [graph u]
  (let [cut-edges (filter #(members? [u] [(:u %) (:v %)]) (graph :edges))]
    (assoc graph  :vertices (disj (graph :vertices) u)
                  :edges    (difference (graph :edges) cut-edges))))

(defmethod remove-vertex DiGraph [graph u]
  (let [cut-edges (filter #(members? [u] [(:to %) (:from %)]) (graph :edges))]
    (assoc graph  :vertices (disj (graph :vertices) u)
                  :edges    (difference (graph :edges) cut-edges))))
