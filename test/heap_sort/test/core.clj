(ns heap-sort.test.core
  (:use [heap-sort.core])
  (:use [clojure.test]))

(deftest sorts-the-same
  (is (= (heap-sort (comparator >) (shuffle (range 100)))
         (sort (comparator >) (shuffle (range 100))))))

(deftest sorts-the-same-2
  (is (= (heap-sort (shuffle (range 100)))
         (sort (shuffle (range 100))))))

(defmacro my-time [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         end# (. System (nanoTime))]
     (- end# start#)))

(deftest first-faster
  (let [rand-seq (shuffle (range 100000))
        first-time (my-time (first (sort rand-seq)))
        second-time (my-time (first (heap-sort rand-seq)))]
    (prn (float (/ second-time first-time)))
    (is (> 1 (float (/ second-time first-time))))))

(deftest not-too-much-slower
  (let [rand-seq (shuffle (range 10000))
        first-time (my-time (vec (sort rand-seq)))
        second-time (my-time (vec (heap-sort rand-seq)))]
    (prn (float (/ second-time first-time)))
    (is (> 5 (float (/ second-time first-time))))))