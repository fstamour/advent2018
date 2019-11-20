
(ns day04)

(require '[clojure.test :refer :all]
         '[clojure.pprint :as pp])

(defn first-non-nil [seq]
  (first
   (filter #(not (nil? %)) seq)))

(defn include-keyword* [keywords string]
  (first-non-nil
   (map
    #(when (clojure.string/includes? string (name %)) %)
    keywords)))

(defn extract-integers-from-string [string]
  (map #(Integer/parseInt %) (re-seq #"\d+" string)))

(comment
  (include-keyword* [:begins :wakes :falls]
                    "[1518-11-01 00:00] Guard #10 begins shift"))

;; [year month day hour minute & guard-id]

(defn parse-log [log]
  (let [action (include-keyword* [:begins :wakes :falls] log)
        [year month day hour minute & [guard-id]] (extract-integers-from-string log)]
    [guard-id ;; is probably null
     action
     [year month day hour minute]]))


(comment
  (map
   parse-log
   (clojure.string/split-lines
    "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up"))
  ;; =>
  ([:begins (1518 11 1 0 0 10)]
   [:falls (1518 11 1 0 5)]
   [:wakes (1518 11 1 0 25)]))

(with-test
  (defn propagate-guard-id [logs]
    (loop [i 0
           current-key (get-in list [i 0])
           list logs]
      (if (< i (count list))
        (let [next-key (or (get-in list [i 0]) current-key)]
          (recur (inc i)
                 next-key
                 (update-in list [i 0]
                            (fn [_] next-key))))
        list)))
  (is (=
       (propagate-guard-id
        [[:x 1]
         [nil 2]
         [:y 3]
         [nil 4]])
       [[:x 1]
        [:x 2]
        [:y 3]
        [:y 4]])))


(defn read-logs
  "Returns the logs, in chronological order.
  Each log entry has the form [guard-id eaction timestamp]
The guard-ids are integers.
The actions are a keyword in [:begins :wakes :falls].
The timestamps are vector with the form [year month day hour minute]."
  ([] (read-logs (slurp "input")))
  ([string]
   (propagate-guard-id
      (into []
            (sort-by #(get % 2)
                     (map parse-log (clojure.string/split-lines string)))))))

(comment (time (read-logs)))
;; => 9ms

(def ^:dynamic *example*
  (read-logs
   "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"))

;; For the first part of the day04 challenge, we only care about the minute's
;; part of the logs.

(defn keep-odd [seq]
  (keep-indexed #(when (odd? %1) %2) seq))
(defn keep-even [seq]
  (keep-indexed #(when (even? %1) %2) seq))

(defn remove-begins [logs]
  (remove (fn [[_ action & _]] (= :begins action)) logs))

(defn check-logs [logs]
  "Some sanity-checks"
  (let [logs (remove-begins logs)]
    ;; Make sure the logs are alternating :falls and :wakes
    (map #(assert (= :falls (get % 1)))
         (keep-even logs))
    (map #(assert (= :wakes (get % 1)))
         (keep-odd logs))
    ;; Also making sure that all pairs are for the same guard.
    (map (fn [[id1 id2]]
           (assert (= id1 id2) (pp/cl-format nil "Ids differs ~a != ~a" id1 id2)))
         (partition 2
                    (map #(get % 0) logs))))
  logs)

(check-logs (read-logs))

(defn get-key-by-greatest-value [dict]
  "Takes a dictionary, sort by value, return the key of the greatest entry"
  (->> dict
       (into [])
       (sort-by #(get % 1))
       last
       first))

;;; Find which guards sleeps the most
(defn guard-who-sleeps-the-most [logs]
  (->> logs
       ;; keep only relevant information
       remove-begins
       (map (fn [[id action [y m d h minute]]] [id minute]))
       check-logs

       ;; compute the time slept by guard
       (partition 2)
       (map (fn [[[id1 from] [id2 to]]]
              {id1 (- to from)}))
       (apply merge-with +)

       ;; get the id of the guard that slept the most
       get-key-by-greatest-value))

(guard-who-sleeps-the-most *example*)
;; => 10, good
(guard-who-sleeps-the-most (read-logs))
;; => 641

;;; Find which minute that guard slept the most often

(defn look [x]
  "Helper to be used with ->>"
  (println x)
  x)

(defn find-frequency-that-guard-slept-on-minute [logs guard-id]
  (->> logs
       ;; keep only relevant information
       (filter #(= (get % 0) guard-id))
       remove-begins
       check-logs
       (map (fn [[id action [y m d h minute]]] minute))

       ;; compute every minute that guard was asleep
       (partition 2)
       (map #(apply range %))
       flatten

       ;; find how many times he slept on each minute.
       frequencies))

(defn find-minute-that-guard-slept-most [logs guard-id]
  (get-key-by-greatest-value
   (find-frequency-that-guard-slept-on-minute logs guard-id)))

(let [logs (read-logs) ;; *example*
      guard-id
      (guard-who-sleeps-the-most logs)
      minute (find-minute-that-guard-slept-most logs guard-id)]
  (* guard-id minute))
;; => 26281


;;; Part 2

(defn find-frequency-that-each-guard-slept-on-minute [logs]
  (->> logs
       (into #{} (map #(get % 0)))
       (map (fn [id] [id
                      (find-frequency-that-guard-slept-on-minute logs id)]))
       (into {})))

(defn all-guard-ids [logs]
  (into #{} (map #(get % 0) logs)))

(let [logs (read-logs)
      guard-ids (into [] (all-guard-ids logs))
      minute-most-slept (map #(find-minute-that-guard-slept-most logs %) guard-ids)
      guard-minute (into {}
                         (map (fn [& xs] (vec xs)) guard-ids minute-most-slept))
      guard-id (get-key-by-greatest-value guard-minute)]
  [ * guard-id (get guard-minute guard-id)])

27995

