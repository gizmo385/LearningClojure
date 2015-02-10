(ns graphs.typed-graph
  (:require
    [clojure.core.typed :as t :refer [U Any Bool ann-record Set ann defalias Num]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Various utility functions
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(t/ann not-nil? [Any -> Bool])
(def not-nil? (complement nil?))

(defmacro defmethods [name dispatch-values args implementation]
  "Defines a method for each dispatch value that uses the provided implementation"
  `(do
     ~@(for [value dispatch-values]
         `(defmethod ~name ~value ~args ~implementation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Defining vertices
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(t/ann-record Vertex [key :- Any, value :- Any])
(defrecord Vertex [key value]
  Object
  (hashCode [u] (-> u :key hash))
  (equals [u v] (== (u :key) (v :key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
; Defining edges
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(t/defalias Edge (U SimpleEdge DirectedEdge WeightedEdge))

; A simple edge just wraps two vertices
(t/ann-record SimpleEdge [u :- Vertex, v :- Vertex])
(defrecord SimpleEdge [u v])

; A directed edge wraps two vertices such that an edge travels from src to dest
(t/ann-record DirectedEdge [src :- Vertex, dest :- Vertex])
(defrecord DirectedEdge [src dest])

; A weighted edge wraps two vertices and assigns a weight to their connection
(t/ann-record WeightedEdge [u :- Vertex, v :- Vertex, weight :- AnyInteger])
(defrecord WeightedEdge [u v weight])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Defining different graph types
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(t/defalias Graph (U SimpleGraph DirectedGraph Network))

; A simple graph has a set of edges and vertices
(t/ann-record SimpleGraph [vertices :- (Set Vertex), edges :- (Set SimpleEdge)])
(defrecord SimpleGraph [vertices edges])

; Edges in a directed graph (DiGraph) explicitly travel from one vertex to another
(t/ann-record DirectedGraph [vertices :- (Set Vertex), edges :- (Set DirectedEdge)])
(defrecord DirectedGraph [vertices edges])

; A edges in a network have a real number weight attached to themselves
(t/ann-record Network [vertices :- (Set Vertex), edges :- (Set WeightedEdge),
                       weight-function :- (t/IFn [Vertex -> Num])])
(defrecord Network [vertices edges weight-function])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Graph implementations
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(t/ann size [Graph -> AnyInteger])
(defmulti size type)

(defmethods size (SimpleGraph DirectedGraph Network) [graph]
  (count (set (:vertices graph))))

(t/ann order [Graph -> AnyInteger])
(defmulti order type)

(defmethods size (SimpleGraph DirectedGraph Network) [graph]
  (count (set (:edges graph))))

(t/ann add-vertices [Graph Any * -> Graph])
(defmulti add-vertices (fn [graph & pairs] (type graph)))

(t/ann remove-vertex [Graph Any -> Graph])
(defmulti remove-vertex (fn [graph key] (type graph)))

(t/ann add-edges [Graph Vertex * -> Graph])
(defmulti add-edges (fn [graph & pairs] (type graph)))

(t/ann find-key [Graph Any -> Vertex])
(defmulti find-key (fn [graph key] (type graph)))

(t/ann degree [Graph Vertex -> AnyInteger])
(defmulti degree (fn [graph vertex] (type graph)))

(t/ann degrees [Graph -> (ASeq AnyInteger)])
(defmulti degrees type)

(defmethods degrees (SimpleGraph DirectedGraph Network) [graph]
  (map (partial degree graph) (:vertices graph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Functions which establish graph invariants
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(t/ann eulerian? [Graph -> Bool])
(defn eulerian? [graph]
  (every? even? (degrees graph)))

(t/ann traversible? [Graph -> Bool])
(defn traversible? [graph]
  (== 2 (count (filter odd? (degrees graph)))))

(t/ann complete? [Graph -> Bool])
(defn complete? [graph]
  (let [required-degree (dec (size graph))]
    (every? (partial == required-degree ) (degrees graph))))
