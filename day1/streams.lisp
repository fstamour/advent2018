;;;; Trying out SICP's stream (a.k.a. thunk-based lazy-lists)

(in-package :cl-user)

(defun memoize-nullary-function (function)
  (let ((already-run? nil)
        (result nil))
    (lambda ()
      (if (not already-run?)
          (progn
            (setf already-run? t
                  result (funcall function)))
          result))))

#+test
(let ((f (memoize-nullary-function (lambda () (print 42)))))
  (loop :for i :below 10 :collect (funcall f)))

(defmacro delay (&body expressions)
  `(memoize-nullary-function (lambda () ,@expressions)))

(defun force (delayed-object)
  (funcall delayed-object))

#+test
(loop
  :with f = (delay (print 42))
  :for i :below 10 :collect (force f))

(defconstant +empty+ '+empty+
  "the empty seq")

(defmacro cons-seq (a &body body) `(cons ,a (delay ,@body)))
(defun singleton (a) (cons-seq a +empty+))
(defun empty? (seq) (eq +empty+ seq))
(defun head (seq) (car seq))
(defun tail (seq) (force (cdr seq)))

(defun range (&optional (from 0) to)
  (if (or (null to)
          (< from to))
      (cons-seq from (range (1+ from) to))
      +empty+))

(defun append-seq (&rest seq-list)
  (if seq-list
    (if (empty? (first seq-list))
        (apply #'append-seq (rest seq-list))
        (cons-seq (head (first seq-list))
          (apply #'append-seq (tail (first seq-list)) (rest seq-list))))
    +empty+))

#+test
(seq-list
 (append-seq (range 0 10) (range 10 20)))

(defun take (n seq)
  "return 2 values: the list of element taken and how many item were taken
e.g. (take 10 (range 0 4)) => (0 1 2 3), 4"
  (let ((acc nil))
    (do ((i 1 (1+ i))
         (it seq (tail it)))
        ((if (empty? it)
             t
           (progn
                (push (head it) acc)
                (>= i n)))
         (values (nreverse acc) (if (= i n)
                                    n
                                    (1- i)))))))

#+test
((take 10 (range 0 0))
 (take 10 (range 0 4))
 (take 1 (range 0 4))
 (take 4 (range 0 4)))

;; alternative implementation
#+nil
(defun take (n seq)
  (loop
    :for i :below n
    :for it = seq :then (tail it)
    :until (empty? it)
    :collect (head it)))

#+test
(take 4 (range 10 20))

(defun seq-list (seq)
  "transform a sequence in a list"
  (loop
    :for it = seq :then (tail it)
    :until (empty? it)
    :collect (head it)))

(defun list-seq (list)
  (if list
      (cons-seq (car list)
        (list-seq (cdr list)))
      +empty+))

#+test
((seq-list (list-seq '(a b c)))
 (list-seq '(a b c))
 (list-seq nil))

;; recursive
(defun drop (n seq)
  (if (empty? seq)
      +empty+
      (if (zerop n)
          (cons-seq (head seq) (tail seq))
          (drop (1- n) (tail seq)))))

;; iterative
(defun drop (n seq)
  (loop :for i :below n
        :unless (empty? seq)
          :do (setf seq (tail seq)))
  seq)


#+test
(take 10 (range))

#+test
(take 4 (drop 10 (range)))


;; map : fn -> list seq -> seq

(defun filter (predicate seq)
  (cond ((empty? seq) +empty+)
        ((funcall predicate (head seq))
         (cons-seq (head seq)
           (filter predicate (tail seq))))
        (t (filter predicate (tail seq)))))

#+test
(seq-list
 (filter #'oddp (range 0 100)))

(defun map-seq (fn &rest seq-list)
  (if (some #'empty? seq-list)
      +empty+
      (cons-seq
          (apply fn (mapcar #'head seq-list))
        (apply #'map-seq fn (mapcar #'tail seq-list)))))

#+test
(seq-list
 (map-seq (lambda (x) (* x x))
         (range 0 10)))

#+test
(seq-list
 (map-seq (lambda (x y) (+ x y))
         (range 0 10)
         (range 10 20)))

#+test
(accumulate #'+ 0 (range 0 10))

(defun accumulate (reducer initial-value seq)
  (let ((acc initial-value))
    (loop
      :for it = seq :then (tail it)
      :until (empty? it)
      :do (setf acc (funcall reducer acc (head it))))
    acc))

(defun accumulation (reducer initial-value seq)
  (if (empty? seq)
      +empty+
      (let ((el (funcall reducer initial-value (head seq))))
        (cons-seq el
          (accumulation reducer el (tail seq))))))

#+test
(seq-list
 (accumulation #'+ 0 (range 0 10)))

(defun flatten (seq)
  (accumulate #'append-seq +empty+ seq))


(defun range-of-range (n &optional (from 0) to)
  (if (zerop n)
      +empty+
      (cons-seq (range from to)
        (range-of-range (1- n) from to))))

#+nil
(seq-list
 (flatten (range-of-range 4 0 3)))

(defun flatmap (fn &rest seq-list)
  (flatten (apply #'map-seq fn seq-list)))
;; todo test ^^^


(defun window (n seq)
  (if (empty? seq)
      +empty+
      (let ((run (take n seq)))
        (if (= n (length run))
            (cons-seq run
              (window n (tail seq)))
            +empty+))))

#+test ((seq-list (window 2 (range 0 10)))
        (seq-list (window 3 (range 0 10)))
        (seq-list (range 0 1))
        (take 2 (range 0 1)))

(defun sum (list) (reduce #'+ list))

#+nil
(seq-list
 (map-seq #'sum
          (window 2 (range 0 10))))

(defconstant +eof+ (gensym "eof"))

(defun read-stream-as-seq (stream)
  (let ((el (read stream nil +eof+)))
    (if (eq el +eof+)
        +empty+
        (cons-seq el (read-stream-as-seq stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~&~d~%"
        (with-open-file (input "input")
          (let ((seq (read-stream-as-seq input)))
            (accumulate #'+ 0 seq))))
;; => 477

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cycle (seq &optional (original seq))
  (if (empty? seq)
      (if (empty? original)
          (cons-seq nil (cycle +empty+))
          (cons-seq (head original) (cycle (tail original) original)))
      (cons-seq (head seq) (cycle (tail seq) original))))

#+test
((take 10 (cycle +empty+))
 (take 10 (cycle (range 0 3))))

(defun find-duplicate (seq)
  (let ((seen (make-hash-table)))
    (flet ((seen? (x) (gethash x seen))
           (seen (x) (setf (gethash x seen) t)))
      (filter #'(lambda (x)
                  (or (seen? x)
                      (not
                       (seen x))))
              seq))))

#+test
(head
 (find-duplicate (list-seq '(a b c a e f g a b))))

(format t "~&~d~%"
        (with-open-file (input "input")
          (let ((seq (read-stream-as-seq input)))
            (head
             (find-duplicate
              (accumulation #'+ 0 (cycle seq)))))))
;; => 390

