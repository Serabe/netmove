(ns netmove
  (:use clojure.contrib.graph)
  (:use vijual)
  (:use [clojure.contrib.def :only (defalias)]))

(defstruct weighted-graph
  :nodes          ; The nodes of the graph, a collection
  :neighbors      ; A function relating nodes and their neighbors
  :weights        ; A function relating and edge and its weight
  :default-weight ; The weight of a non-existant edge
  )

(def weights (accessor weighted-graph :weights))

(def default-weight (accessor weighted-graph :default-weight))

(def nodes (accessor weighted-graph :nodes))

(defn- get-all-arcs
  "Devuelve todos los arcos de g"
  [g]
  (for [x (nodes g) y (get-neighbors g x)] [x y]))

(defn draw
  "Dado un grafo lo dibuja en pantalla. Utiliza la librería vijual."
  [g]
  (draw-directed-graph (get-all-arcs g)))

(defn get-weight

  "Obtiene el peso del arco (i j) en g.
   Si no existe, devuelve default-weight."
  
  [g i j]
  (if (= i j)
    0
    (let [wi (get (weights g) i {})]
      (get wi j (default-weight g)))))

(defalias gw get-weight)

(defn get-node-by-num
  "Devuelve el i-ésimo nodo de g. Acepta un tercer argumento opcional con el valor devuelto si no existe el nodo."
  ([g i]
     ((nodes g) i))
  ([g i not-existant]
     (try (get-node-by-num g i)
          (catch Exception e
            not-existant))))

(defalias gnbn get-node-by-num)

(defn get-number-of-node
  "Devuélve el índice del nodo i en g."
  [g i]
  (let [nds (nodes g)
        n2i-map (zipmap nds (range (count nds)))]
    (n2i-map i)))

(defalias gnon get-number-of-node)

(defn get-weight-by-ordinal
  "Es como get-weight sólo que en vez del arco (i,j) se le pasa el ordinal de i y de j."
  [g i j]
  (let [nodes (nodes g)]
    (if (= i j)
      0
      (get-weight g (gnbn g i) (gnbn g j)))))

(defalias gwbo get-weight-by-ordinal)

(defn mtr-adj
  "Dado g devuelve la matriz de adyacencia."
  [g]
  (let [n (.length (nodes g))
        mtr (make-array Object n n)]
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

; Funciones no relacionadas con grafos útiles para ambos algoritmos

(defn range-without
  "Devuelve rng sin los elementos pasados."
  [rng & args]
  (remove (set args) rng))

(defn join-paths
  "Dados dos caminos, p y s, tal que el vértice final de p es el inicial de s, devuelve el camino unión."
  [p s]
  (into (rest s) (reverse p)))

(defn extended-min
  "Comparación equivalente a <, pero añadiendo los casos correspondientes para poder manejar :infty como marca de infinito"
  [p q]
  (cond
   (= p :infty) false ; Si p es infinito, p no puede ser estrictamente
                      ; menor que nada.
   (= q :infty) (not (= p :infty)) ; Si q es infinito, basta que p no
                                   ; lo sea para que sea estrictamente menor
   :else (< p q)) ; Si ninguno es infinito, usamos <
  )

(defn extended-add
  "Adición equivalente a la suma, pero capaz de manejar infinitos (sólamente infinitos positivos)"
  [l c]
  (if (or (= l :infty) (= c :infty))
    :infty
    (+ l c)))

; Funciones para el algoritmo Bellman-Ford.

(defn stop-fn-bf
  "Función de parada del algoritmo Bellman-Ford"
  [prev act vs i]
  (every? #(= (prev %) (act %)) vs))

(defn make-checker-bf
  "Crea un comprobador general. Simplemente comprueba que la longitud del camino no sea :infty."
  [g i l p]
  (fn [path]
    (not (= :infty (l (gnon g (first path))))))) ;first porque el
                                        ;camino lo devuelve invertido.

(defn- init-l-bf
  "Inicializa el vector l del algoritmo Bellman-Ford para el grafo g y el vértice i.
i viene dado por un número."
  [g i]
  (let [l (make-array Object (count (nodes g)))]
    (doseq [n (range (count (nodes g)))]
      (if (= i n)
        (aset l n  0)
        (aset l n (gwbo g i n))))
    l))

(defn- init-p-bf
  "Inicializa el vector p del algoritmo Bellman-Ford de dimensión dim, cuyo vértice inicial es i (dado por el índice)."
  [dim i]
  (let [p (make-array Integer/TYPE dim)]
    (doseq [j (range dim)]
      (aset p j i))
    p))

(defn- retrieve-path-bf
  ([g p e]
      (let [ne (gnbn g e)]
        (if (= e (p e))
          (list ne)
          (cons ne (retrieve-path-bf g p (p e))))))
  ([checker g p e]
     (let [path (retrieve-path-bf g p e)]
       (if (checker path)
         path
         []))))

(defn bf-solution-to-fn
     "Dado un grafo y la solución de bf-gral, devuelve una función que espera un nodo y devuelve el camino óptimo junto con su longitud. Además, la solución no es nula."
     [g sol]
     (fn [node]
       (let [non (gnon g node)]
         {:length ((sol 0) non)
          :path   ((sol 1) non)})))

(defn- bellman-ford-impl
  "Calcula el camino más corto desde el vértice i al resto.
   g es el grafo.
   i es el nodo inicial.
   stop-function es un predicado que recibe:
    1.- Función para obtener el valor previo dado el vértice.
    2.- Función para obtener el valor actual dado el vértice.
    3.- La colección de vértices.
    4.- El nodo inicial tal como está en 3.
  comp es la función de comparación. Ha de ser transitiva. Recibe dos longitudes de caminos en g.
  add es la función de adición de una longitud con un arco. Recibe como primer parámetro la longitud de un camino y como segundo el peso de un arco."
  [g i comp add]
  (let [noi (gnon g i)                  ; noi stands for number of i
        nds (nodes g)
        nds-count (count nds)
        nds-range (range nds-count)]
    (loop [lprev (init-l-bf g noi)
           lact  (aclone lprev)
           p (init-p-bf nds-count noi)
           q 1]
      (doseq [j (range-without nds-range noi)]
        (let [k (arg-comp comp #(if (= % j) (default-weight g)  (add (aget lprev %) (gwbo g % j))) nds-range)
              min (add (get lprev k) (gwbo g k j))]
          (if (not (comp (aget lprev j) min))
            (do
              (aset lact j (add (aget lprev k) (gwbo g k j)))
              (aset p j k))
            (aset lact j (aget lprev j)))))
      (cond
       (comp (reduce #(if (comp %1 %2) %1 %2) (map #(add (aget lprev %) (gwbo g % noi)) (range-without nds-range noi))) 0) nil
       (every? #(= (aget lprev %) (aget lact %)) nds-range) [lact, p]
       (= q nds-count) nil
       true (recur lact lact p (inc q))))))

(defn bellman-ford-gral
  "Calcula el camino más corto desde el vértice i al resto.
   make-checker es una función que dados el grafo g, el  nodo i, el vector l y el vector p devuelve la función checker de retrieve-path-bf
   g es el grafo.
   i es el nodo inicial.
   comp es la función de comparación. Ha de ser transitiva. Recibe dos longitudes de caminos en g.
  add es la función de adición de una longitud con un arco. Recibe como primer parámetro la longitud de un camino y como segundo el peso de un arco."
  [make-checker g i comp add]
  (let [sol (bellman-ford-impl g i comp add)]
    (if (nil? sol)
      (fn [node] nil)
      (let [ls (vec (sol 0))
            ps (vec (sol 1))
            checker (make-checker g i ls ps)
            paths (vec (map #(vec (reverse (retrieve-path-bf checker g ps %))) (range (count (nodes g)))))]
        (bf-solution-to-fn g [ls paths])))))

(defn bellman-ford
  "Bellman-Ford standard."
  [g i]
  (bellman-ford-gral make-checker-bf g i extended-min extended-add))

(defalias bf bellman-ford)

; Floyd-Warshall

(defn- init-l-fw
  "Inicializa el vector l para el algoritmo Floyd-Warshall"
  [g]
  (mtr-adj g))

(defn- init-p-fw
  "Inicializa el vector p para el algoritmo Floyd-Warshall"
  [g]
  (let [dim (count (nodes g))
        p (make-array Integer/TYPE dim dim)
        rng (range dim)]
    (doseq [i rng]
      (doseq [j rng]
        (aset p i j i)))
    p))

(defn- retrieve-path-fw
  "Devuelve el camino en g de i a j especificado por el array p.
   i y j están especificados por un ordinal.
   Se añade un parámetro opcional al principio. Éste ha de ser una función que dados los vértices i y j, devuelva true si el camino existe y false si no."
  ([g p i j]
     (let [k ((p i) j)]
        (if (= i k)
          (if (= i j)
            [(gnbn g i)]
            (vec (list (gnbn g i) (gnbn g j))))
          (vec (join-paths (retrieve-path-fw g p i k) (retrieve-path-fw g p k j))))))
  ([checker g p i j]
     (let [path (retrieve-path-fw g p i j)]
       (if (checker path)
         path
         []))))

(defn make-checker-fw
  "Crea un comprobador general. Simplemente comprueba que (l ij) no sea :infty."
  [g l p]
  (fn [path]
    (not (= :infty ((l (gnon g (last path))) (gnon g (first path)))))))

(defn fw-solution-to-fn
  [g sol]
  (fn [i j]
    (let [noi (gnon g i)
          noj (gnon g j)]
      {:length (((sol 0) noi) noj)
       :path   (((sol 1) noi) noj)})))

(defn- floyd-warshall-impl
  "Aplica el algoritmo Floyd-Warshall al grafo g. Las funciones comp y add son equivalentes a las encontradas en bf-gral"
  [g comp add]
  (let [nds (nodes g)
        nds-cnt (count nds)
        nds-range (range nds-cnt)
        nds-range-without (partial range-without nds-range)]
    (loop [l (init-l-fw g)
           p (init-p-fw g)
           k 0]
      (if (some #(comp (add (aget l % k) (aget l k %)) 0) (nds-range-without k)) ;condición de parada
        (fn [i j] nil)
        (do
          (doseq [i (nds-range-without k)]
            (doseq [j (nds-range-without k i)]
              (let [lij (aget l i j)
                    lik (aget l i k)
                    lkj (aget l k j)
                    likj (add lik lkj)]
                (when (comp likj lij)
                  (aset l i j likj)
                  (aset p i j (aget p k j))))))
          (if (= k (dec nds-cnt))
            [l p]
            (recur l p (inc k))))))))

(defn floyd-warshall-gral
  [make-checker g comp add]
  (let [sol (floyd-warshall-impl g comp add)]
    (if (nil? sol)
      (fn [i j] nil)
      (let [l         (vec (map vec (vec (sol 0))))
            p         (vec (map vec (vec (sol 1))))
            nds-range (range (count (nodes g)))
            checker   (make-checker g l p)
            paths     (vec (for [x nds-range] (vec (for [y nds-range] (vec (retrieve-path-fw checker g p x y))))))]
        (fw-solution-to-fn g [l paths])))))

(defn floyd-warshall
  [g]
  (floyd-warshall-gral make-checker-fw g extended-min extended-add))

(defalias fw floyd-warshall)
