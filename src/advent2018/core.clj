(ns advent2018.core)

(defn reload  []  (use 'advent2018.data :reload)  (use 'advent2018.core :reload))

(defmacro tryprob
  [num part]
  `(do
     (reload)
     (~(symbol (str  "problem" num "_p" part))  ~(symbol (str  "data" num)))))

; --------------------------------------------------------------------------
; ----->  Code for solutions <----------------------------------------------
; --------------------------------------------------------------------------

(defn problem1_p1
  ([numstr]
   (problem1_p1 0 (clojure.string/split numstr #"\s+")))
  ([sofar numstrs]
   (if (= 0 (count numstrs))
     sofar
     (let [strlen (count (first numstrs))
           int (if (> strlen 0) (Integer/parseInt (first numstrs)) 0)]
       (recur (+ sofar int) (rest numstrs))))))

(defn problem1_p2
  ([numstr]
   (problem1_p2 0 #{0} (cycle (->> (clojure.string/split numstr #"\s+") (map read-string)))))
  ([lastfreq sofar numstrs]
   (let [int (first numstrs)
         freq (+ lastfreq int)]
     (if (contains? sofar freq)
       freq
       (recur freq (conj sofar freq) (rest numstrs))))))

(defn containstwice
  [in]
  (let [char (-> in first str)]
    (if (= 0 (count in))
      false
      (let [starting (clojure.string/index-of in char 1)]
        (if (and (some? starting)
                 (nil? (clojure.string/index-of in char (+ 1 starting))))
          true
          (recur (apply str (filter #(not (= % (first in))) (rest in)))))))))

(defn containsthrice
  [in]
  (let [char (-> in first)]
    (if (= 0 (count in))
      false
      (let [starting (clojure.string/index-of in char 1)
            second (if (some? starting) (clojure.string/index-of in char (+ 1 starting)) nil)]
        (if (and (some? starting)
                 (some? second)
                 (nil? (clojure.string/index-of in char (+ 1 second))))
          true
          (recur (apply str (filter #(not (= % char)) (rest in)))))))))

(defn problem2_p1
  ([numstr]
   (problem2_p1 {:two 0 :three 0} (clojure.string/split-lines numstr)))
  ([status numstrs]
   (if (= 0 (count numstrs))
     (* (:two status) (:three status))
     (let [dut (first numstrs)
           add2 (containstwice dut)
           add3 (containsthrice dut)
           new2 (if add2 (+ 1 (:two status)) (:two status))
           new3 (if add3 (+ 1 (:three status)) (:three status))]
       (recur {:two new2 :three new3} (rest numstrs))))))

(defn hamming-distance
  [a b]
  (->> (map = a b)
       (filter false?)
       count))

(defn find-hamming-distance-of-one
  [in]
  (let [dut (first in)
        trythis (filter #(= 1 (hamming-distance dut %)) in)]
    (if (< 0 (count trythis))
      (concat [dut] trythis)
      (recur (rest in)))))

(defn rm-!=
  ([a b]
   (rm-!= "" a b))
  ([rV a b]
   (if (or (= 0 (count a)) (= 0 (count b)))
     rV
     (let [newRV (if (= (first a) (first b))
                   (apply str (str rV (first a)))
                   rV)]
       (recur newRV (->> a rest (apply str)) (->> b rest (apply str)))))))

(defn problem2_p2
  ([numstr]
   (let [newstrs (-> numstr clojure.string/split-lines)
         goodstrs (-> newstrs
                      find-hamming-distance-of-one)
         eqchars (map = (first goodstrs) (second goodstrs))
         badcharloc (.indexOf eqchars false)]
     (rm-!= (first goodstrs) (second goodstrs)))))
