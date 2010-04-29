(ns netmove.prueba
  (:use (clojure.contrib graph)))

(defn camino-desde
  ([g o te] (camino-desde g o te ()))
  ([g o te vis]
     (if (te o)
       (cons o vis)
       (for [o2 (get-neighbors g o) :when (not (some #(= o2 %) vis))] (camino-desde g o2 te (cons o vis))))))

(comment
  (def g (struct directed-graph ['A 'B 'C 'D 'E] {'A ['B 'C 'D] 'B ['C] 'C ['A 'D] 'D ['C] 'E []}))

  (camino-desde g 'A #(= % 'D))
  ; ((((D C B A))) ((D C A)) (D A))
  )
