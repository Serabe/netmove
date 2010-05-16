(ns netmove_test
  (:use clojure.test
        netmove))

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

(deftest test-get-weight
  (is (= 0 (get-weight g 'A 'A)))
  (is (= (default-weight g) (get-weight g 'E 'F)))
  (is (= 50 (get-weight g 'C 'D))))

(deftest test-get-weight-by-ordinal
  (is (= (default-weight g) (get-weight-by-ordinal g 4 5)))
  (is (= 50 (get-weight-by-ordinal g 2 3))))

(deftest test-get-node-by-num
  (is (= 'A (get-node-by-num g 0))))
