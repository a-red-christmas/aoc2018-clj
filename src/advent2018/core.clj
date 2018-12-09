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
    answer))

(defn problem4_p2 [str-in]
  (let [ctx (p4-build-context str-in)
        ctxlog (dissoc (-> ctx :log) :none)
        guardnums (map p4-guard-summary ctxlog)
        winner (last (sort-by #(-> % :most second) guardnums))
        answer (* (:guard winner) (-> winner :most first))]
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

(defn p6-manh-dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn p6-which-closest [coord target]
  (let [tmpmap (map
                 (fn [a]
                   {a
                    (p6-manh-dist a target)}) coord)
        winner (first (sort (group-by #(-> % vals first) tmpmap)))
        loser? (< 1 (-> winner second count))
        ]
    (if loser?
      nil
      (-> winner second first first first))
    ))

(defn p6-sum-dists [coord target]
  (let [adds (reduce + (map #(p6-manh-dist % target) coord)) ]
    adds
    ))

(defn problem6_p1 [str-in]
  (let [locs (for [line (clojure.string/split-lines str-in)]
               (map read-string (clojure.string/split line #"[,\s]+")))
        max-x (apply max (map first locs))
        max-y (apply max (map second locs))
        bad-points (concat
                     (for [x (range (+ 1 1 max-x))] [x (+ 1 max-y)])
                     (for [x (range (+ 1 1 max-x))] [x 0])
                     (for [y (range (+ 1 1 max-y))] [0 y])
                     (for [y (range (+ 1 1 max-y))] [(+ 1 max-x) y]))
        all-points (zipmap (for [x (range (+ 1 1 max-x))
                                 y (range (+ 1 1 max-y))]
                             [x y])
                           (cycle [nil]))
        done-maps (map (fn [a] {(first a) (p6-which-closest locs (first a))}) all-points)
        done-map (reduce merge done-maps)
        infinite-closers (distinct (map #(get done-map %) bad-points))
        all-closests (-> done-map vals frequencies)
        valid-closests (reduce dissoc all-closests infinite-closers)
        ]
      (->> valid-closests vals (apply max))))

(defn problem6_p2 [str-in]
  (let [locs (for [line (clojure.string/split-lines str-in)]
               (map read-string (clojure.string/split line #"[,\s]+")))
        max-x (apply max (map first locs))
        max-y (apply max (map second locs))
        all-points (zipmap (for [x (range (+ 1 1 max-x))
                                 y (range (+ 1 1 max-y))]
                             [x y])
                           (cycle [nil]))
        done-maps (filter #(> 10000 %) (map #(p6-sum-dists locs (first %)) all-points))
        ]
      (count done-maps)))

(defn linewise [str-in]
  (clojure.string/split-lines str-in))

(defn p7-get-node [line]
  (let [[first second] (->
                         line
                         (clojure.string/split #" must be finished before step "))
        newfirst (-> first (subs 5) keyword)
        newsecond (-> second (subs 0 1) keyword)
        ]
    {:need newfirst :toget newsecond}))

(defn p7-reduce-graph [sofar cur]
  (assoc sofar
         (:toget cur)
         (into
           (get sofar (:toget cur) #{})
           [(:need cur)])))

(defn problem7_p1 [str-in]
  (let [lines (linewise str-in)
        nodes (map p7-get-node lines)
        all-keys (reduce #(into %1 [(:need %2) (:toget %2)]) #{} nodes)
        usable-map (reduce p7-reduce-graph (zipmap (vec all-keys) (cycle [#{}])) nodes)
        ]
    (loop [rV '()
           curmap usable-map]
      (if (= 0 (count curmap))
        (->> rV (map name) (apply str))
        (let [togive (first
                       (sort
                         (map first
                              (filter
                                #(= 0 (count (-> % second)))
                                curmap))))
              newmap (apply merge (map (fn [a] {(-> a first) (disj (-> a second) togive)}) (dissoc curmap togive)))]
          (recur (concat rV [togive]) newmap))))))

(defn p7-remove-give [curmap togive]
  (if togive
    (apply merge (map (fn [a] {(-> a first) (disj (-> a second) togive)}) (dissoc curmap togive)))
    curmap))

(defn problem7_p2
  ([str-in]
   (problem7_p2 str-in 5 60))
  ([str-in numworkers minduration]
   (let [indexes [:A :B :C :D :E :F :G :H :I :J :K :L :M :N :O :P :Q :R :S :T :U :V :W :X :Y :Z]
         lines (linewise str-in)
         nodes (map p7-get-node lines)
         all-keys (reduce #(into %1 [(:need %2) (:toget %2)]) #{} nodes)
         usable-map (reduce p7-reduce-graph (zipmap (vec all-keys) (cycle [#{}])) nodes)
         ]
     (loop [rV '()
            curmap usable-map
            workers []
            numticks 0]
       (if (= 0 (count curmap) (count workers))
         (dec numticks)
         (let [
               newworkers (map #(assoc % :dur (-> % :dur dec))
                               (filter #(-> % :dur (!= 0)) workers))
               viable-givers (sort (map first (filter #(= 0 (count (-> % second))) curmap)))
               togive (take (- numworkers (count newworkers)) viable-givers)
               nextworkers (reduce
                             #(concat %1 [{:dur (+ minduration (.indexOf indexes %2)) :what %2}])
                             newworkers
                             togive)
               newmap (reduce #(dissoc %1 %2) curmap togive)
               ready (filter #(-> % :dur (= 0)) nextworkers)
               newrV (concat rV (sort (map :what ready)))
               newmap (reduce #(p7-remove-give %1 %2) newmap (map :what ready))
               ]
           (recur newrV newmap nextworkers (inc numticks))))))))

(defn p8-proc-ray
  ([ray]
   (->
     (p8-proc-ray 1 (first ray) (second ray) (drop 2 ray))
     :data))
  ([myid num metadata rayleft]
   (loop [id (inc myid)
          still-children num
          rV {:metadata [] :children {}}
          total-consumed metadata
          curray rayleft]
     (if (= 0 still-children)
       {:id id
        :consumed total-consumed
        :data (assoc rV :metadata (take metadata curray))}
       (let [from-child (p8-proc-ray id (first curray) (second curray) (drop 2 curray))
             newrV (assoc-in rV [:children id] (:data from-child))
             newid (:id from-child)
             sub-consumed (:consumed from-child)]
         (recur newid
                (dec still-children)
                newrV
                (+ sub-consumed 2 total-consumed)
                (drop (+ 2 sub-consumed) curray)))))))

(defn p8-sum-metadata [data-in]
  (let [cursum (reduce + (:metadata data-in))]
    (apply + cursum (for [[id data] (-> data-in :children)] (p8-sum-metadata data)))))

(defn p8-node-val [data]
  (let [kids (->> data :children vec sort (map second))
        metadata (-> data :metadata)
        all-vals (for [which metadata]
                   (if (<= which (count kids))
                     (p8-node-val (nth kids (dec which)))
                     0))]
    (if (= 0 (count kids))
      (reduce + metadata)
      (reduce + all-vals))))

(defn problem8_p1 [str-in]
  (let [ray (map read-string (clojure.string/split str-in #"\s+"))
        data (p8-proc-ray ray)
        metasum (p8-sum-metadata data)]
    metasum))

(defn problem8_p2 [str-in]
  (let [ray (map read-string (clojure.string/split str-in #"\s+"))
        data (p8-proc-ray ray)
        metasum (p8-node-val data)]
    metasum))
