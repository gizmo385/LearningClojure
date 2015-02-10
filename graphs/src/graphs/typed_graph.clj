(ns graphs.typed-graph
  (:require
    [clojure.core.typed :as t :refer [U Any Bool ann-record Set ann defalias Num]]))

; Utility functions
(ann not-nil? [Any -> Bool])
(def not-nil? (complement nil?))

; Vertex definition
(ann-record Vertex [key :- Any, value :- Any])
(defrecord Vertex [key value])

; Edge definition
(defalias Edge (U SimpleEdge WeightedEdge DirectedEdge))

(ann-record SimpleEdge [u :- Vertex, v :- Vertex])
(defrecord SimpleEdge [u v])

(ann-record WeightedEdge [u :- Vertex, v :- Vertex, weight :- Num])
(defrecord WeightedEdge [u v weight])

(ann-record DirectedEdge [to :- Vertex, from :- Vertex])
(defrecord DirectedEdge [to from])

; Graph definitions
(defalias Graph (U SimpleGraph Network ))

(ann-record SimpleGraph [vertices :- (Set Vertex), edges :- (Set SimpleEdge)])
(defrecord SimpleGraph [vertices edges])

(ann-record Network [vertices :- (Set Vertex), edges :- (Set WeightedEdge),
                     weight-function  :- [WeightedEdge -> Num]])
(defrecord Network [vertices edges weight-function])

(ann-record DiGraph [vertices :- (Set Vertex), edges :- (Set DirectedEdge)])
(defrecord DiGraph [vertices edges])


(t/defprotocol GraphBase
  (vertices [graph :- Graph] :- (Set Vertex))
  (edges [graph :- Graph] :- (Set Edge))
  (add-vertex [graph :- Graph vertex :- Vertex] :- Graph))

(defn [Testing] Fuck you ())
