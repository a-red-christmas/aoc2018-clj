(ns advent2018.core)

(defn reload  []  (use 'advent2018.data :reload)  (use 'advent2018.core :reload))

(defn != [a b] (not (= a b)))

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

(defn p3-parse-line
  [line]
  (let [toks (clojure.string/split line #"\s+")
        num (->> toks first rest (apply str) read-string)
        [startx starty] (map read-string (-> toks (get 2) (clojure.string/split #"[,:]")))
        [claimx claimy] (map read-string (-> toks (get 3) (clojure.string/split #"x")))]
    (merge (zipmap
             (for [x (map #(+ 1 startx %) (range claimx))
                   y (map #(+ 1 starty %) (range claimy))]
               (keyword (str x "|" y)))
             (cycle [num]))
           {num (* claimx claimy)})))

(defn all-merges-contested
  [a b]
  :contested)

(defn problem3_p1 [str-in]
  (let [lines (clojure.string/split-lines str-in)
        all-regions (map p3-parse-line lines)
        merged-regions (reduce #(merge-with all-merges-contested %1 %2) all-regions)
        contested-num (->> merged-regions vals (filter #(= % :contested)) count)]
    contested-num))

(defn problem3_p2 [str-in]
  (let [lines (clojure.string/split-lines str-in)
        all-regions (map p3-parse-line lines)
        merged-regions (apply merge-with all-merges-contested all-regions)
        all-vals (-> merged-regions vals frequencies)
        vals-which-are-keys (filter #(not (keyword? %)) (-> merged-regions keys))
        got-correct-num (filter #(= (get merged-regions %)
                                    (-> all-vals (get %))) vals-which-are-keys)]
    got-correct-num))

(defn p4-parse-what [what]
  (let [toks (rest (clojure.string/split what #"\s+"))]
    (case (first toks)
      "falls" :asleep
      "wakes" :awake
      "Guard" (-> toks second (subs 1) read-string)
      :err)))

(defn p4-parse-line [ctx line]
  (let [[_ time what] (clojure.string/split line #"[\[\]]")
        [date hourtime] (clojure.string/split time #"\s+")
        minute (Integer/parseInt (second (clojure.string/split hourtime #"[:]")))
        parsed-what (p4-parse-what what)]
    (case parsed-what
      :asleep (update-in ctx [:on-duty :status] conj {:time minute :wake? :asleep})
      :awake (update-in ctx [:on-duty :status] conj {:time minute :wake? :awake})
      (-> ctx
          (update-in [:log (-> ctx :on-duty :id) :status] conj [(-> ctx :on-duty :clock-in) (-> ctx :on-duty :status)])
          (assoc-in [:on-duty :id] parsed-what)
          (assoc-in [:on-duty :status] [])
          (assoc-in [:on-duty :clock-in] [date hourtime])))))

(defn p4-get-one-watch [watch]
  (loop [ctx {}
         last-time 0
         state :awake
         curtimes watch]
    (if (= 0 (count curtimes))
      (merge ctx
             (zipmap (range last-time 60) (cycle [(if (= state :awake) 0 1)])))
      (let [curtime (first curtimes)
            intVal (if (= state :awake) 0 1)
            new-time (-> curtime :time)
            new-state (-> curtime :wake?)
            newctx (merge ctx
                          (zipmap (range last-time new-time) (cycle [intVal])))]
        (recur newctx new-time new-state (rest curtimes))))))

(defn p4-to-60-minutes [guard-status]
  (loop [ctx {:clock-in [] :watchlog {}}
         logs guard-status]
    (if (= 0 (count logs))
      ctx
      (let [msg (first logs)
            in-time (first msg)
            wakelog (sort-by :time (second msg))
            new-watchlog (merge-with +
                                     (-> ctx :watchlog)
                                     (p4-get-one-watch wakelog))
            new-clock-in (conj (-> ctx :clock-in) in-time)]
        (recur {:clock-in new-clock-in :watchlog new-watchlog} (rest logs))))))

(defn p4-get-total-minutes-sleeping [guard]
  (reduce + (-> guard :watchlog vals)))

(defn p4-get-minute-most-sleeping [guard]
  (apply max-key #(val %) (->> guard :watchlog)))

(defn p4-guard-summary [guardlogs]
  (let [mins (p4-to-60-minutes (-> guardlogs second :status))
        total-sleeping (p4-get-total-minutes-sleeping mins)
        highest-sleeping (p4-get-minute-most-sleeping mins)]
    {:guard (first guardlogs)
     :total total-sleeping
     :mins mins
     :most highest-sleeping}))

(defn p4-build-context [str-in]
 (let [lines (sort (conj (clojure.string/split-lines str-in) "[2018-11-01 00:00] Guard #-5 begins shift"))]
    (loop [ctx {:on-duty {:id :none}}
           linesleft lines]
      (if (= 0 (count linesleft))
        ctx
        (recur (p4-parse-line ctx (first linesleft)) (rest linesleft))))))

(defn problem4_p1 [str-in]
  (let [ctx (p4-build-context str-in)
        ctxlog (dissoc (-> ctx :log) :none)
        guardnums (map p4-guard-summary ctxlog)
        winner (last (sort-by :total guardnums))
        answer (* (:guard winner) (-> winner :most first))]
    (prn (first guardnums))
    answer))

(defn problem4_p2 [str-in]
  (let [ctx (p4-build-context str-in)
        ctxlog (dissoc (-> ctx :log) :none)
        guardnums (map p4-guard-summary ctxlog)
        winner (last (sort-by #(-> % :most second) guardnums))
        answer (* (:guard winner) (-> winner :most first))]
    (prn (first guardnums))
    (prn winner)
    answer))

(defn problem5_p1 [str-in]
  (let [matchex #"(aA|Aa|bB|Bb|cC|Cc|dD|Dd|eE|Ee|fF|Ff|gG|Gg|hH|Hh|iI|Ii|jJ|Jj|kK|Kk|lL|Ll|mM|Mm|nN|Nn|oO|Oo|pP|Pp|qQ|Qq|rR|Rr|sS|Ss|tT|Tt|uU|Uu|vV|Vv|wW|Ww|xX|Xx|yY|Yy|zZ|Zz)"]
    (loop [prevstr ""
           prcme str-in]
      (if (= (count prevstr) (count prcme))
        (count prcme)
        (recur prcme (clojure.string/replace prcme matchex ""))))))

(defn problem5_p2 [str-in]
  (loop [tryremove '(#"a|A" #"b|B" #"c|C" #"d|D" #"e|E" #"f|F" #"g|G" #"h|H" #"i|I" #"j|J" #"k|K" #"l|L" #"m|M" #"n|N" #"o|O" #"p|P" #"q|Q" #"r|R" #"s|S" #"t|T" #"u|U" #"v|V" #"w|W" #"x|X" #"y|Y" #"z|Z")
         shortest-so-far (count str-in)]
    (if (= 0 (count tryremove))
      shortest-so-far
      (let [next-best (problem5_p1 (-> str-in (clojure.string/replace (first tryremove) "")))]
        (recur (rest tryremove) (min shortest-so-far next-best))))))
