(ns ^{:doc "Defines an interface between Titanium and Loom."
      :author "Horst Duchene"}
    archimedes-loom.core
  (:require [clojurewerkz.archimedes.graph :as g]
            [clojurewerkz.archimedes.io :as io]
            [clojurewerkz.archimedes.vertex :as v]
            [clojurewerkz.archimedes.edge :as e]
            [loom.graph :refer :all]
            [loom.io :refer :all]))

(defn arch->loom-digraph
  "Converts archimedes graph into Loom representation"
  [arch-graph]
  (let [edges-set (set (.getEdges arch-graph))]
    (-> (digraph)
        (add-nodes* (.getVertices arch-graph))
        (add-edges* (map e/endpoints edges-set)))))

(defn load-graph-gml [file]
  (let [result (g/clean-tinkergraph)]
    (io/load-graph-graphml result file)
    result))

(defn loom->arch-graph [lg]
  (let [result (g/clean-tinkergraph)
        nodemap (reduce (fn [map n]
                          (assoc map n (v/create-with-id! result n {:Text n})))
                        {}
                        (nodes lg))]
    (doseq [[from to] (edges lg)]
      (if-not (= from to)
        (e/connect! result (get nodemap from) "" (get nodemap to))))
    result))

(defn write-graph-gml [g file]
  (let []
    (io/write-graph-graphml g file)))

(defn arch-view [g]
  (loom.io/view (arch->loom-digraph g)
                :node-label v/to-map
                :edge-label
                (fn [n1 n2]
                  (mapv e/to-map
                        (e/edges-between n1 n2)))))
