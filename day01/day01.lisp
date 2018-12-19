
(in-package :cl-user)

;;; Utils

(defun read-input ()
  "Read the input as a list."
  (with-open-file (input "input")
    (loop :for x = (read input nil)
          :while x
          :collect x)))

;; Make sure the printer doesn't loop infinitly on circular linked list
(setf *print-circle* t)

(defun circular (items)
  "Make a circular list out of a proper list."
  (setf (cdr (last items)) items)
  items)


;;; 1

;; Read and loop
(format t "~&~d~%"
        (with-open-file (input "input")
          (loop :for x = (read input nil)
                :while x
                :sum x)))
;; => 477

;; Read then reduce
(format t "~&~d~%"
        (reduce #'+ (read-input)))
;; => 477


;;; 2

(format t "~&~d~%"
        (let ((already-seen (make-hash-table)))
          (flet ((seen? (x) (gethash x already-seen))
                 (seen (x) (setf (gethash x already-seen) t)))
            (loop :for i :in (circular
                              (read-input))
                  :sum i :into sum
                  :if (seen? sum) :return sum
                    :else :do (seen sum)))))
;; => 390

