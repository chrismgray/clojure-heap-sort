(ns heap-sort.core)

(defn- left-child-elt
  {:inline (fn [elt]
             `(+ 1 (* 2 ~elt)))}
  [^long elt]
  (+ 1 (* 2 elt)))

(defn- right-child-elt
  {:inline (fn [elt]
             `(+ 2 (* 2 ~elt)))}
  [^long elt]
  (+ 2 (* 2 elt)))

(defn- ^java.lang.Object left-child
  ([^objects coll ^long elt]
     (aget coll (left-child-elt elt))))

(defn- ^java.lang.Object right-child
  ([^objects coll ^long elt]
     (aget coll (right-child-elt elt))))

(defn- left-child-exists?
  {:inline (fn [coll elt len]
             `(> ~len (left-child-elt ~elt)))}
  ([^objects coll ^long elt ^long len]
     (> len (left-child-elt elt))))

(defn- right-child-exists?
  {:inline (fn [coll elt len]
             `(> ~len (right-child-elt ~elt)))}
  ([^objects coll ^long elt ^long len]
     (> len (right-child-elt elt))))

(defn- exchange [^objects coll ^long e1 ^long e2]
  (let [new-e1 (aget coll e2)
        new-e2 (aget coll e1)]
    (aset coll e1 new-e1)
    (aset coll e2 new-e2)
    coll))

(defn ^long max-index
  ([^java.util.Comparator comp ^objects coll ^long e1 ^long e2]
     (if (> 0 (comp (aget coll e1) (aget coll e2)))
       e1
       e2)))

(defn- heap-fix [^java.util.Comparator comp ^objects coll ^long len ^long elt]
  (cond
   (right-child-exists? coll elt len)
   (let [best-index-1 (int (max-index comp coll elt (left-child-elt elt)))
         best-index (int (max-index comp coll best-index-1 (right-child-elt elt)))]
     (if (== best-index elt)
       coll
       (recur comp (exchange coll elt best-index) len best-index)))
   (left-child-exists? coll elt len)
   (let [best-index (int (max-index comp coll elt (left-child-elt elt)))]
     (if (== best-index elt)
       coll
       (recur comp (exchange coll elt best-index) len best-index)))
   :else
   coll))

(defn pop-heap [^java.util.Comparator comp heap ^long len]
  [(first heap) (heap-fix comp (exchange heap 0 (dec len)) (dec len) 0)])

(defn build-heap
  ([coll]
     (build-heap compare coll))
  ([^java.util.Comparator comp coll]
     (let [len (count coll)
           first-index (int (/ len 2))
           a (to-array coll)
           f (partial heap-fix comp a len)]
       (loop [i first-index]
         (if (>= i 0)
           (do
             (f i)
             (recur (dec i)))
           a)))))

(defn heap-sort
  ([coll]
     (heap-sort compare coll))
  ([^java.util.Comparator comp coll]
     (let [heap (build-heap comp coll)
           len (count coll)
           helper (fn helper [^objects heap ^long len]
                    (if (= 0 len)
                      nil
                      (let [[f hp] (pop-heap comp heap len)]
                        (lazy-seq (cons f (helper hp (dec len)))))))]
       (helper heap len))))
