;;;; nix-shell -p boot; boot repl

(ns day01)

;;; Utils

(defn read-input []
  (with-open [stream (clojure.java.io/reader "input")]
    (into []
          (map read-string (line-seq stream)))))

;;;; #1

(println
 (reduce + (read-input)))

;;;; #2

;; not the nicest code :/
(defn find-first-duplicate [s]
  (get
   (first
    (filter (fn [[_ already-present? el]] already-present?)
            (reductions (fn [[coll _ _] el]
                          [(conj coll el) (not (nil? (get coll el))) el])
                        [#{} nil nil]
                        s)))
   2))

(println
 (find-first-duplicate
  (reductions +
              (cycle (read-input)))))

