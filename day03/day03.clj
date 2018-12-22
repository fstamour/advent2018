(ns day03)

(require '[clojure.test :refer :all]
         '[clojure.set])

(with-test
  (defn parse-claim [claim]
    (let [[id x y w h]
          (map read-string (re-seq #"\d+" claim))]
      (map #(assert (and
                     (not (nil? %))
                     (integer? %)))
           [id x y w h])
      {:id id :x x :y y :w w :h h}))
  (is (= {:id 7 :x 1 :y 3 :w 4 :h 5}
         (parse-claim "#7 @ 1,3: 4x5"))))

(defn read-input []
  (with-open [stream (clojure.java.io/reader "input")]
    (into []
          (map parse-claim (line-seq stream)))))

(defn range* [from n]
  (range from (+ from n)))

(defn surface [claim]
  "Return a sequence of points covered by the claim"
  (for [x (range* (:x claim) (:w claim))
        y (range* (:y claim) (:h claim))]
    [x y]))

(defn greater-than-1 [x]
  (> x 1))

(println
 (->> (for [claim (read-input)]
        (surface claim))
      (apply concat)
      (frequencies)
      (vals)
      (filter greater-than-1)
      (count)))
;; => 115304


;;; Part-two

(defn find-overlapping-claim-ids [claims]
  (->>
   ;; Generate a dictionary, key = coordinate, value = set of ids
   (for [claim claims]
     (for [[x y] (surface claim)]
       {[x y] #{(:id claim)}}))
   (apply concat)
   (apply merge-with clojure.set/union)
   ;; Keep only the entries in the dictionary that have more that 2 ids.
   (filter #(-> %
                val
                count
                greater-than-1))
   ;; Convert to set of ids
   vals
   (apply clojure.set/union)))

(println
 (let [claims (read-input)
       ids (into #{} (map :id claims))]
   (clojure.set/difference ids (find-overlapping-claim-ids claims))))
;; => #{275}


