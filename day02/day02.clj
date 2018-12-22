(ns day02)

(require '[clojure.test :refer :all])

;; , t a   to run-all-tests with spacemacs + cider.

;;;; Checksums

(with-test
  (defn frequencies' [frequency-map]
    "Takes the output of the `frequencies` function.
Returns the set of frequencies of 2s and 3s."
    (into #{}
          (for [[key freq] frequency-map :when (< 1 freq 4)]
            freq)))
  (is (= #{} (frequencies' {})))
  (is (= #{} (frequencies' {:a 1 :b 4})))
  (is (= #{2} (frequencies' {:a 2 :b 2})))
  (is (= #{3} (frequencies' {:a 3})))
  (is (= #{2 3} (frequencies' {:a 3 :b 2}))))

(with-test
  (defn checksum [frequency-sets]
    "Compute the checksum from the output of `frequencies'`"
    (reduce *
            (vals
             (frequencies
              (mapcat #(if (= #{2 3} %)
                         [2 3]
                         [(first %)])
                      frequency-sets)))))
  (is (= 12 (checksum [#{2} #{2 3} #{2} #{3} #{2} #{3}]))))

(comment
  (reduce *
          (vals
           (frequencies
            (mapcat #(if (= #{2 3} %)
                       [2 3]
                       [(first %)]))))))

(println
 (with-open [stream (clojure.java.io/reader "input")]
   (checksum
    (map (comp frequencies' frequencies)
         (line-seq stream)))))
;; => 6944


(defn count-if [predicate sequence]
  "Count the number of element that passes a certain predicate."
  (count (filter predicate sequence)))

(defn count-differences [seq1 seq2]
  "Iterate "
  (count-if (fn [x] (not x))
            (map = seq1 seq2)))

;; (count-differences "abc" "akc") => 1

(defn find-close-sequences [sequences]
  (into #{}
        (for [x sequences
              y sequences
              :when (and (not (= x y))
                         (= 1 (count-differences x y)))]
          #{x y})))

(defn find-common-elements [seq1 seq2]
  (remove nil?
          (map #(when (= %1 %2) %1) seq1 seq2)))

;; (find-common-elements "abc" "akc")
;; => (\a \c)

(defn day02-part2 [ids]
  (clojure.string/join
   (apply find-common-elements
          (into []
                (first (find-close-sequences ids))))))

;; Example from the problem statement.
;; (day02-part2 '["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])
;; => "fgij"

(let [;; Get all ids as a vector
      ids (with-open [stream (clojure.java.io/reader "input")]
            (into [] (line-seq stream)))]
  (println
   (day02-part2 ids)))
;; => "srijafjzloguvlntqmphenbkd"

