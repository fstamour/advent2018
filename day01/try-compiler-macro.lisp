;;;; Trying out compiler-macros

(load "day01-streams.lisp")

(defun range-form-p (form)
  (and
   (listp form)
   (eq (first form) 'range))
  ;; TODO Maybe check the length
  )

(defun analyze-range-form (form)
  "Take a form like (range [from [to]]) and returns 3 values:
optimizable-p from to"
  (when (range-form-p form)
    (case (length form)
      (1 (values t 0))
      (2 (values (numberp (second form)) (second form)))
      (3 (values (and (numberp (second form))
                      (numberp (third form)))
                 (second form)
                 (third form))))))

#+test
(loop :for (form expected-result) :on
      '(x (nil)
        (list) (nil)
        (range) (t 0)
        (range 42) (t 42)
        (range 42 100) (t 42 100)
        (range x) (nil x)
        (range x 0) (nil x 0)
        (range 0 y) (nil 0 y)
        (range x y) (nil x y))
      :by #'cddr
      :for result = (multiple-value-list
                     (analyze-range-form form))
      :unless (equalp result expected-result)
        :append (list (format nil "~&Test failed for input \"~A\", expected \"~A\", got \"~A\".~%"
                              form expected-result result)))

(defun drop-form-p (form)
  "Does the form match (drop _ _)"
  (and
   (listp form)
   (eq (first form) 'drop)
   (eq (length form) 3)))

(defun drop-range-form-p (form)
  (and (drop-form-p form)
       (range-form-p (third form))))

(defun analyze-drop-range (form)
  (when (and
         (drop-range-form-p form)
         (numberp (second form)))
    (multiple-value-bind (optimizable-range-p from to)
        (analyze-range-form (third form))
      (when optimizable-range-p
        (values t (+ from (second form)) to)))))

#+test
(every #'null
       (analyze-drop-range 'x)
       (analyze-drop-range '(list))
       (analyze-drop-range '(drop x))
       (analyze-drop-range '(drop x (range)))
       (analyze-drop-range '(drop x (range 2)))
       (analyze-drop-range '(drop x (range 2 4)))
       (analyze-drop-range '(drop 2 (range x 4)))
       (analyze-drop-range '(drop 2 (range x y))))

#+test
(loop :for (form expected-result) :on
      '((drop 2 (range)) (t 2 nil)
        (drop 2 (range 2)) (t 4 nil)
        (drop 2 (range 2 4)) (t 4 4))
      :by #'cddr
      :for result = (multiple-value-list
                     (analyze-drop-range form))
      :unless (equalp result expected-result)
        :append (list (format nil "~&Test failed for input \"~A\", expected \"~A\", got \"~A\".~%"
                              form expected-result result)))

(defun take-form-p (form)
  "Does the form match (take _ _)"
  (and
   (listp form)
   (eq (first form) 'take)
   (eq (length form) 3)))

(defun take-range-form-p (form)
  (and (take-form-p form)
       (range-form-p (third form))))

;; We got the semipredicate problem right here.
;; We should return 2 values to differentiate between a form that can't be optimized
;; and a form that can be optimized to nil (optimized away).
(defun analyze-take-form (form)
  (when (and (take-form-p form)
             (numberp (second form)))
    (destructuring-bind (take n seq)
        form
      (declare (ignore take))
      (cond ((range-form-p (third form))
             (multiple-value-bind (optimizable-range-p from to)
                 (analyze-range-form seq)
               (when optimizable-range-p
                 (let ((to (if to (min to n) n)))
                   (when (< from to)
                     (loop :for i :from from :to to
                           :collect i))))))))))
(every #'null
       (analyze-take-form 'x)
       (analyze-take-form '(list))
       (analyze-take-form '(take))
       (analyze-take-form '(take 12))
       (analyze-take-form '(take x ()))
       (analyze-take-form '(take x (range)))
       (analyze-take-form '(take x (range 1)))
       (analyze-take-form '(take x (range 1 2))))

(analyze-take-form '(take 0 (range))) '(0 1)
(analyze-take-form '(take 1 (range))) '(0 1)
(analyze-take-form '(take 1 (range 1 2))) '(1)
(analyze-take-form '(take 1 (range 1 2))) '(1)


(define-compiler-macro take (&whole form n seq)
  (format t "~&n: ~a~%seq: ~a~%form: ~a~%" n seq form)
  (cond
    ((not (numberp n)) form)
    ((take-range-form-p form) )
    (t form))
  (if (and (numberp n)
           (listp seq))
      (cond
        ;; (take n (range [m]))
        ((eq (first seq) 'range)
         (let ((start (or (cadr seq) 0)))
           (if (numberp start)
               `(quote ,(loop
                          :for i :from  :below n
                          :collect i))
               ;; Can't optimize (range x) we don't know what x is.
               from)))
        ;; (take n (drop m (range [p]))
        ((and (eq (first seq) 'drop)
              (numberp )
              (listp (second seq))
              (eq (first (second seq)) 'range))
         `(quote ,(loop
                    :for i :from (+
                                  ()
                                  (or (cadr seq) 0)) :below n
                    :collect i)))
        (t form))))

(with-output-to-string (*standard-output*)
  (take 10 (range)))

(take 10 (range 5))

(with-output-to-string (*standard-output*)
  (take 4 (drop 10 (range))))
;; => 10 11 12 13
