(ns heap-sort.core)

(defn- left-child-elt
  "Find the left child in the heap of a given element."
  {:inline (fn [elt]
             `(+ 1 (* 2 ~elt)))}
  [^long elt]
  (+ 1 (* 2 elt)))

(defn- right-child-elt
  "Find the right child in the heap of a given element."
  {:inline (fn [elt]
             `(+ 2 (* 2 ~elt)))}
  [^long elt]
  (+ 2 (* 2 elt)))

(defn- ^java.lang.Object left-child
  "Get the left child in heap `coll` of element at position `elt`."
  ([^objects coll ^long elt]
     (aget coll (left-child-elt elt))))

(defn- ^java.lang.Object right-child
  "Get the right child in heap `coll` of element at position `elt`."
  ([^objects coll ^long elt]
     (aget coll (right-child-elt elt))))

(defn- left-child-exists?
  "Determines whether the left child of the element at position `elt`
   in the heap `coll` exists."
  {:inline (fn [coll elt len]
             `(> ~len (left-child-elt ~elt)))}
  ([^objects coll ^long elt ^long len]
     (> len (left-child-elt elt))))

(defn- right-child-exists?
  "Determines whether the right child of the element at position `elt`
   in the heap `coll` exists."
  {:inline (fn [coll elt len]
             `(> ~len (right-child-elt ~elt)))}
  ([^objects coll ^long elt ^long len]
     (> len (right-child-elt elt))))

(defn- exchange [^objects coll ^long e1 ^long e2]
  "Swaps elements at positions `e1` and `e2` in the heap `coll`."
  (let [new-e1 (aget coll e2)
        new-e2 (aget coll e1)]
    (aset coll e1 new-e1)
    (aset coll e2 new-e2)
    coll))

(defn ^long max-index
  "Returns the index which is larger (according to `comp`) between
   `e1` and `e2` in `coll`."
  ([^java.util.Comparator comp ^objects coll ^long e1 ^long e2]
     (if (> 0 (comp (aget coll e1) (aget coll e2)))
       e1
       e2)))

(defn- heap-fix
  "Treating the array `coll` as a binary tree (where the left child of
   element `elt` is at 2`elt` + 1 and the right child is at 2`elt` +
   2), we cause `coll` to have the heap property, which is that each
   element is greater than all the elements in its subtrees.  This
   'fix' works only for trees where the subtrees are heaps and the
   root node is possibly smaller one of its children."
  [^java.util.Comparator comp ^objects coll ^long len ^long elt]
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

(defn pop-heap
  "Take the top of the heap and return it, along with a fixed heap
   with the top removed."
  [^java.util.Comparator comp heap ^long len]
  [(first heap) (heap-fix comp (exchange heap 0 (dec len)) (dec len) 0)])

(defn build-heap
  "Build a heap in linear time given a collection `coll` and optionally a
   comparator `comp`."
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
  "Returns a lazy sorted sequence of the input sequence `coll`,
   optionally compared by the comparator `comp`.  Does a linear amount
   of preprocessing, followed by O(\log n) work per element that is
   taken from the sorted sequence.  Thus, this function has better
   performance characteristics than Clojure's standard `sort` function
   if only a few sorted elements are desired.  If all elements are
   desired, it tends to be slightly worse, due to the overhead of
   creating the lazy sequence."
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
