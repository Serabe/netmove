(ns netmove.prueba
  (:use (clojure.contrib graph)))

(defstruct weighted-graph
  :nodes          ; The nodes of the graph, a collection
  :neighbors      ; A function relating nodes and their neighbors
  :weights        ; A function relating and edge and its weight
  :default-weight ; The weight of a non-existant edge
  )

(defn esta-en? [e coll]
  (some #(= e %) coll))

(def g (struct weighted-graph
               ['A 'B 'C 'D 'E]
               { ; Neighbors
                'A ['B 'C 'D]
                'B ['C]
                'C ['A 'D]
                'D ['C]
                'E []}
               { ; Weights
                'A {'B 56 'C 6 'D 2}
                'B {'C -3}
                'C {'A -100 'D 50}
                'D {'C 43}
                'E {}}
               1000000))

(defn get-weight [g i j]
  (let [w (:weights g)
        dw (:default-weight g)]
    (if (contains? w i)
      (let [w2 (w i)]
        (if (contains? w2 j)
          (w2 j) dw))
      dw)))

(defn camino-desde-con-peso
  ([g o te fc pm] (camino-desde-con-peso g o te fc pm ()))
  ([g o te fc pm vis]
     (if (te o)
       (cons o vis)
       (for [o2 (get-neighbors g o) :when (and (fc o2 vis) (< (get-weight g o o2) pm))]
         (camino-desde-con-peso g o2 te fc (- pm (get-weight g o o2)) (cons o vis))))))

(defn camino-desde
  ([g o te fc] (camino-desde g o te fc ()))
  ([g o te fc vis]
     (if (te o)
       (cons o vis)
       (for [o2 (get-neighbors g o) :when (fc o2 vis)] (camino-desde g o2 te fc (cons o vis))))))
