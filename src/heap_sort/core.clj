(ns heap-sort.core)

(defn- comp-max
  ([^java.util.Comparator comp x] x)
  ([^java.util.Comparator comp x y] (if (> 0 (comp x y)) x y))
  ([^java.util.Comparator comp x y z] ; here for speed only
     (if (> 0 (comp x y))
       (if (> 0 (comp x z))
         x
         z)
       (if (> 0 (comp y z))
         y
         z)))
  ([^java.util.Comparator comp x y z & more]
     (second
      (reduce
       (fn [[comp x] y]
         (if (> 0 (comp x y))
           [comp x]
           [comp y])) [comp (comp-max comp x y z)] more))))

(defn- left-child
  ([^Integer elt]
     (inc (* 2 elt)))
  ([coll ^Integer elt]
     (aget coll (left-child elt))))

(defn- right-child
  ([^Integer elt]
     (inc (inc (* 2 elt))))
  ([coll ^Integer elt]
     (aget coll (right-child elt))))

(defn- ^Boolean left-child-exists?
  {:inline (fn [coll elt len]
             `(> ~len (left-child ~elt)))}
  ([coll ^Integer elt ^Integer len]
     (> len (left-child elt))))

(defn- ^Boolean right-child-exists?
  {:inline (fn [coll elt len]
             `(> ~len (right-child ~elt)))}
  ([coll ^Integer elt ^Integer len]
     (> len (right-child elt))))

(defn- exchange [coll ^Integer e1 ^Integer e2]
  (let [new-e1 (aget coll e2)
        new-e2 (aget coll e1)]
    (aset coll e1 new-e1)
    (aset coll e2 new-e2)
    coll))

(defn- heap-fix [^java.util.Comparator comp coll ^Integer elt ^Integer len]
  (let [comp-fn #(comp (first %1) (first %2))]
   (cond
    (and (left-child-exists? coll elt len) (right-child-exists? coll elt len))
    (let [max-elt (comp-max comp-fn [(aget coll elt) elt] [(left-child coll elt) (left-child elt)] [(right-child coll elt) (right-child elt)])]
      (if (= (second max-elt) elt)
        coll
        (recur comp (exchange coll elt (second max-elt)) (second max-elt) len)))
    (left-child-exists? coll elt len)
    (let [max-elt (comp-max comp-fn [(aget coll elt) elt] [(left-child coll elt) (left-child elt)])]
      (if (= (second max-elt) elt)
        coll
        (recur comp (exchange coll elt (second max-elt)) (second max-elt) len)))
    :else
    coll)))

(defn pop-heap [^java.util.Comparator comp heap ^Integer len]
  [(first heap) (heap-fix comp (exchange heap 0 (dec len)) 0 (dec len))])

(defn build-heap
  ([coll]
     (build-heap compare coll))
  ([^java.util.Comparator comp coll]
     (let [len (count coll)
           first-index (int (/ len 2))
           a (to-array coll)]
       (second
        (reduce (fn [[comp coll] elt] [comp (heap-fix comp coll elt len)]) [comp (to-array coll)] (reverse (range (inc first-index))))))))

(defn heap-sort
  ([coll]
     (heap-sort compare coll))
  ([^java.util.Comparator comp coll]
     (let [heap (build-heap comp coll)
           len (alength heap)
           helper (fn helper [heap len]
                    (if (= 0 len)
                      nil
                      (let [[f hp] (pop-heap comp heap len)]
                        (lazy-seq (cons f (helper hp (dec len)))))))]
       (helper heap len))))

