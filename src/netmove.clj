(ns netmove
  (:use clojure.contrib.graph))

(defstruct weighted-graph
  :nodes          ; The nodes of the graph, a collection
  :neighbors      ; A function relating nodes and their neighbors
  :weights        ; A function relating and edge and its weight
  :default-weight ; The weight of a non-existant edge
  )

(def weights (accessor weighted-graph :weights))

(def default-weight (accessor weighted-graph :default-weight))

(def nodes (accessor weighted-graph :nodes))

(defn get-weight

  "Obtiene el peso del arco (i j) en g.
   Si no existe, devuelve default-weight."
  
  [g i j]
  (let [wi (get (weights g) i {})]
    (get wi j (default-weight g))))

(def get-weight gw)

(defn get-weight-by-ordinal
  "Es como get-weight sólo que en vez del arco (i,j) se le pasa el ordinal de i y de j."
  [g i j]
  (let [nodes (nodes g)]
    (if (= i j)
      0
      (get-weight g (nth nodes i) (nth nodes j)))))

(def gwbo get-weight-by-ordinal)

(comment 
  (defn camino-desde
    "[g o te fc]
   Dado g grafo, o vértice del grafo, calcula todos los caminos
   posibles dada la función de poda te, comprobando con la
   función de contenido fc.
   La función de poda va de vértices en booleanos.
   La función de contenido va de vértices por lista de vértices
   en booleanos."
    ([g o te fc] (camino-desde g o te fc ()))
    ([g o te fc vis]
       (if (te o)
         (cons o ())
         (let [paths (filter (complement empty?) (for [o2 (get-neighbors g o) :when (fc o2 vis)] (camino-desde g o2 te fc (cons o vis))))]
           (reduce into paths))))))

(defn get-node-by-num
  "Devuelve el i-ésimo nodo de g."
  [g i]
  ((nodes g) i))

; alias para get-node-by-num
(def gnbn get-node-by-num)

(defn mtr-adj
  "Dado g devuelve la matriz de adyacencia."
  [g]
  (let [n (.length (nodes g))
        mtr (make-array Double/TYPE n n)]
    (dotimes [i n]
      (dotimes [j n]
        (aset mtr i j (get-weight g (gnbn g i) (gnbn g j)))))
    mtr))

(defn arg-comp
  "Dada f función, dado coll collección, devuelve el índice k tal que (comp f(coll[k]) f(coll[j])) es verdadero, siempre y cuando comp represente una relación transitiva."
  [comp f coll]
  (let [arr (vec coll)
        n (.length arr)
        nf (fn [i] (f (arr i)))]
    (reduce #(if (comp (nf %1) (nf %2)) %1 %2) (range n))))

(def argmin (partial arg-comp <))
(def argmax (partial arg-comp >))

(defn- init-l-bf
  "Inicializa el vector l del algoritmo Bellman-Ford para el grafo g y el vértice i.
i viene dado por un número."
  [g i]
  (let [
        l (make-array Double/TYPE (count (nodes g)))]
    (doseq [n (range (count (nodes g)))]
      (if (= i n) ;posiblemente mal el gnbn
        (aset l n  0)
        (aset l n (gwbo g i n))))
    l)
  ;(vec (map #(if (= % i) 0 (get-weight g i (gnbn g %)) ) (range
                                        ;(count (nodes g)))))
  )

(defn bf-gral
  "Calcula el camino más corto desde el vértice i al resto."
  [g i]
  (let [nodes (range (count (nodes g)))]
    (loop [lprev (init-l-bf g (gnbn g i))
           lact  (make-array Double/TYPE (count (nodes g)))
           q 2]
      (doseq [j nodes]
        (when-not (= (gnbn g j) i)
          (let [min (apply min (map #(+ (aget lprev %) (gwbo g % j)) (remove #(= % j) nodes)))])
          (if (> (aget lprev j) min)
            (let [k (argmin #(+ (aget lprev %) (gwbo g % j)) nodes)]
              (aset lact j (+ (aget lprev k) (gwbo g k j))))
            (aset lact j (aget lprev j)))))
      (if (every? #(= (aget lprev %) (aget lact %)) nodes)
        lact
        (if (= q (count nodes))
          nil 
          (recur lact lact (inc q))))
      )))

(comment
  (def g (struct weighted-graph
               ['A 'B 'C 'D 'E 'F]
               { ; Neighbors
                'A ['B 'C 'D]
                'B ['C]
                'C ['A 'D 'F]
                'D ['C]
                'E []
                'F ['D]}
               { ; Weights
                'A {'B 56 'C 6 'D 2}
                'B {'C -3}
                'C {'A -100 'D 50 'F 60}
                'D {'C 43}
                'E {}
                'F {'D -3}}
               1000000))
  )
