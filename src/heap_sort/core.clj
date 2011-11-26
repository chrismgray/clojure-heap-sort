(ns heap-sort.core)

(defn- comp-max
  ([comp x] x)
  ([comp x y] (if (> 0 (comp x y)) x y))
  ([comp x y & more]
     (second
      (reduce
       (fn [[comp x] y]
         (if (> 0 (comp x y))
           [comp x]
           [comp y])) [comp (comp-max comp x y)] more))))

(defn- left-child
  ([elt]
     (inc (* 2 elt)))
  ([coll elt]
     (coll (left-child elt))))

(defn- right-child
  ([elt]
     (inc (inc (* 2 elt))))
  ([coll elt]
     (coll (right-child elt))))

(defn- left-child-exists? [coll elt]
  (contains? coll (left-child elt)))

(defn- right-child-exists? [coll elt]
  (contains? coll (right-child elt)))

(defn- exchange [coll e1 e2]
  (-> coll
      (assoc-in [e1] (coll e2))
      (assoc-in [e2] (coll e1))))

(defn- heap-fix [comp coll elt]
  (cond
   (and (left-child-exists? coll elt) (right-child-exists? coll elt))
   (let [max-elt (comp-max #(comp (first %1) (first %2)) [(coll elt) elt] [(left-child coll elt) (left-child elt)] [(right-child coll elt) (right-child elt)])]
     (if (= (second max-elt) elt)
       coll
       (recur comp (exchange coll elt (second max-elt)) (second max-elt))))
   (left-child-exists? coll elt)
   (let [max-elt (comp-max #(comp (first %1) (first %2)) [(coll elt) elt] [(left-child coll elt) (left-child elt)])]
     (if (= (second max-elt) elt)
       coll
       (recur comp (exchange coll elt (second max-elt)) (second max-elt))))
   :else
   coll))

(defn pop-heap [comp heap]
  [(first heap) (heap-fix comp (subvec (exchange heap 0 (dec (count heap))) 0 (dec (count heap))) 0)])

(defn build-heap
  ([coll]
     (build-heap compare (vec coll)))
  ([comp coll]
     (let [first-index (int (/ (count coll) 2))]
       (second (reduce (fn [[comp coll] elt] [comp (heap-fix comp coll elt)]) [comp (vec coll)] (reverse (range (inc first-index))))))))

(defn heap-sort
  ([coll]
     (heap-sort compare coll))
  ([^java.util.Comparator comp coll]
     (let [heap (build-heap comp coll)
           helper (fn helper [heap]
                    (if (empty? heap)
                      nil
                      (let [[f hp] (pop-heap comp heap)]
                        (lazy-seq (cons f (helper hp))))))]
       (helper heap))))

