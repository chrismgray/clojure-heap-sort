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

(defn- ^long left-child-elt
  {:inline (fn [elt]
             `(+ 1 (* 2 ~elt)))}
  [^long elt]
  (+ 1 (* 2 elt)))

(defn- ^long right-child-elt
  {:inline (fn [elt]
             `(+ 2 (* 2 ~elt)))}
  [^long elt]
  (+ 2 (* 2 elt)))

(defn- left-child
  ([^java.util.Arrays coll ^long elt]
     (aget coll (left-child-elt elt))))

(defn- right-child
  ([^java.util.Arrays coll ^long elt]
     (aget coll (right-child-elt elt))))

(defn- ^Boolean left-child-exists?
  {:inline (fn [coll elt len]
             `(> ~len (left-child-elt ~elt)))}
  ([^java.util.Arrays coll ^long elt ^long len]
     (> len (left-child-elt elt))))

(defn- ^Boolean right-child-exists?
  {:inline (fn [coll elt len]
             `(> ~len (right-child-elt ~elt)))}
  ([^java.util.Arrays coll ^long elt ^long len]
     (> len (right-child-elt elt))))

(defn- ^java.util.Arrays exchange [^java.util.Arrays coll ^long e1 ^long e2]
  (let [new-e1 (aget coll e2)
        new-e2 (aget coll e1)]
    (aset coll e1 new-e1)
    (aset coll e2 new-e2)
    coll))

(defn ^Integer max-index
  ([^java.util.Comparator comp ^java.util.Arrays coll ^long e1 ^long e2]
     (if (> 0 (comp (aget coll e1) (aget coll e2)))
       e1
       e2))
  ([^java.util.Comparator comp ^java.util.Arrays coll e1 e2 e3]
     (let [e-best (max-index comp coll e1 e2)]
       (max-index comp coll e-best e3))))

(defn- ^java.util.Arrays heap-fix [^java.util.Comparator comp ^java.util.Arrays coll elt len]
  (cond
   (right-child-exists? coll elt len)
   (let [best-index (max-index comp coll elt (left-child-elt elt) (right-child-elt elt))]
     (if (== best-index elt)
       coll
       (recur comp (exchange coll elt best-index) best-index len)))
   (left-child-exists? coll elt len)
   (let [best-index (max-index comp coll elt (left-child-elt elt))]
     (if (== best-index elt)
       coll
       (recur comp (exchange coll elt best-index) best-index len)))
   :else
   coll))

(defn pop-heap [^java.util.Comparator comp ^java.util.Arrays heap ^long len]
  [(first heap) (heap-fix comp (exchange heap 0 (dec len)) 0 (dec len))])

(defn ^java.util.Arrays build-heap
  ([coll]
     (build-heap compare coll))
  ([^java.util.Comparator comp coll]
     (let [len (count coll)
           first-index (int (/ len 2))
           next-first-index (dec first-index)
           a (to-array coll)]
       (second
        (reduce (fn [[comp coll] elt]
                  [comp (heap-fix comp coll (- next-first-index elt) len)])
                [comp a]
                (range first-index))))))

(defn heap-sort
  ([coll]
     (heap-sort compare coll))
  ([^java.util.Comparator comp coll]
     (let [^java.util.Arrays heap (build-heap comp coll)
           len (alength heap)
           helper (fn helper [^java.util.Arrays heap ^long len]
                    (if (= 0 len)
                      nil
                      (let [[f hp] (pop-heap comp heap len)]
                        (lazy-seq (cons f (helper hp (dec len)))))))]
       (helper heap len))))

