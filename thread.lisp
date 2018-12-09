;; Just a little something to kill threads stuck in an infinite loop
(in-package :cl-user)

(defpackage thread (:use cl sb-thread))

(in-package thread)

(loop :for thread in (list-all-threads)
      :unless (eq *current-thread* thread)
        :when (string= "worker" (thread-name thread))
          :do (terminate-thread thread))

