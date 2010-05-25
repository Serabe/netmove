(ns netmove-test
  (:use netmove))

; g1 tiene un ciclo negativo (A C).
; además, se encuentran los caminos más cortos a la vez que el ciclo.
(def g1 (struct weighted-graph
                ['A 'B 'C 'D 'E 'F]
                {                     ; Neighbors
                 'A ['B 'C 'D]
                 'B ['C]
                 'C ['A 'D 'F]
                 'D ['C]
                 'E []
                 'F ['D]}
                {                     ; Weights
                 'A {'B 56 'C 6 'D 2}
                 'B {'C -100}
                 'C {'A -1 'D 50 'F 60}
                 'D {'C 3}
                 'E {}
                 'F {'D -3}}
                1000000))

; g2 no tiene ciclos negativos.
(def g2 (struct weighted-graph
                ['A 'B 'C 'D 'E 'F]
                {                     ; Neighbors
                 'A ['B 'C 'D]
                 'B ['C]
                 'C ['A 'D 'F]
                 'D ['C]
                 'E []
                 'F ['D]}
                {                     ; Weights
                 'A {'B 56 'C 6 'D 2}
                 'B {'C -1}
                 'C {'A -1 'D 50 'F 60}
                 'D {'C 3}
                 'E {}
                 'F {'D -3}}
                1000000))

; g3 es como g2, pero con :infty
(def g3 (struct weighted-graph
                ['A 'B 'C 'D 'E 'F]
                {                     ; Neighbors
                 'A ['B 'C 'D]
                 'B ['C]
                 'C ['A 'D 'F]
                 'D ['C]
                 'E []
                 'F ['D]}
                {                     ; Weights
                 'A {'B 56 'C 6 'D 2}
                 'B {'C -1}
                 'C {'A -1 'D 50 'F 60}
                 'D {'C 3}
                 'E {}
                 'F {'D -3}}
                :infty))
(comment 
  (bf g1 'A)
  (bf g2 'A)
  (bf g3 'A)
  (floyd-warshall g1)
  (floyd-warshall g2)
  (floyd-warshall g3)
  )

(def road-example (struct weighted-graph
                          ['A 'B 'C 'D 'E 'F 'G 'H 'I]
                          {
                           'A ['B 'C 'D]
                           'B ['E 'F]
                           'C ['E]
                           'D ['F]
                           'E ['G 'H]
                           'F ['H]
                           'G ['I]
                           'H ['I]}
                          {
                           'A {'B 3.2 'C 2.8 'D 2.2}
                           'B {'E 2 'F 3.6}
                           'C {'E 3.7}
                           'D {'F 4}
                           'E {'G 2.9 'H 3.5}
                           'F {'H 2.1}
                           'G {'I 3.3}
                           'H {'I 3.8}}
                          0))

(defn bf-road []
  (let [sol  (bellman-ford-gral make-checker-bf road-example 'A > min)
        h    (:length (sol 'I))
        path (:path (sol 'I))]
    (str "La altura máxima es " h " y el camino es " (apply str (interpose ", " path)) "."))
  )
