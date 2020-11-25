;;; simple-parallel-tasks
;;; Copyright 2019-2020 Guillaume Le Vaillant
;;; This library is free software released under the GNU GPL-3 license.

(defpackage :simple-parallel-tasks-tests
  (:use :cl :fiveam :simple-parallel-tasks))

(in-package :simple-parallel-tasks-tests)


(def-suite simple-parallel-tasks-tests)

(in-suite simple-parallel-tasks-tests)

(defmacro duration (&body body)
  (let ((start-time (gensym))
        (end-time (gensym))
        (result (gensym))
        (duration (gensym)))
    `(let* ((,start-time (get-internal-real-time))
            (,result ,@body)
            (,end-time (get-internal-real-time))
            (,duration (/ (- ,end-time ,start-time)
                          internal-time-units-per-second)))
       (values ,duration ,result))))

(test plist
  (is (null (plist)))
  (is (equalp '(a) (plist 'a)))
  (is (equalp '(10 20) (plist (+ 4 6) (* 4 5))))
  (is (equalp '(1 2 3 4 5) (plist 1 2 3 4 5)))
  (is (> 1.01 (duration (plist (sleep 1)))))
  (is (> 1.01 (duration (plist (sleep 1) (sleep 1)))))
  (is (> 1.01 (duration (plist (sleep 1) (sleep 1) (sleep 1))))))

(test pvalues
  (is (null (multiple-value-list (pvalues))))
  (is (equalp '(a) (multiple-value-list (pvalues 'a))))
  (is (equalp '(10 20) (multiple-value-list (pvalues (+ 4 6) (* 4 5)))))
  (is (equalp '(1 2 3 4 5) (multiple-value-list (pvalues 1 2 3 4 5))))
  (is (> 1.01 (duration (multiple-value-list (pvalues (sleep 1))))))
  (is (> 1.01 (duration (multiple-value-list (pvalues (sleep 1) (sleep 1))))))
  (is (> 1.01 (duration
               (multiple-value-list (pvalues (sleep 1) (sleep 1) (sleep 1)))))))

(test plet
  (is (null (plet ())))
  (is (equalp 'a
              (plet ((x 'a))
                x)))
  (is (equalp '(10 20)
              (plet ((x (+ 4 6))
                     (y (* 4 5)))
                (list x y))))
  (is (equalp '(1 2 3 4 5)
              (plet ((a 1)
                     (b 2)
                     (c 3)
                     (d 4)
                     (e 5))
                (list a b c d e))))
  (is (> 1.01 (duration (plet ((x (sleep 1)))
                          x))))
  (is (> 1.01 (duration (plet ((x (sleep 1))
                               (y (sleep 1)))
                          (list x y)))))
  (is (> 1.01 (duration (plet ((x (sleep 1))
                               (y (sleep 1))
                               (z (sleep 1)))
                          (list x y z))))))

(test pprog1
  (is (equalp 'a (pprog1 'a)))
  (is (equalp 10 (pprog1 (+ 4 6) (* 4 5))))
  (is (equalp 1 (pprog1 1 2 3 4 5)))
  (is (> 1.01 (duration (pprog1 (sleep 1)))))
  (is (> 1.01 (duration (pprog1 (sleep 1) (sleep 1)))))
  (is (> 1.01 (duration (pprog1 (sleep 1) (sleep 1) (sleep 1))))))

(test pprogn
  (is (null (pprogn)))
  (is (equalp 'a (pprog1 'a)))
  (is (equalp 20 (pprogn (+ 4 6) (* 4 5))))
  (is (equalp 5 (pprogn 1 2 3 4 5)))
  (is (> 1.01 (duration (pprogn (sleep 1)))))
  (is (> 1.01 (duration (pprogn (sleep 1) (sleep 1)))))
  (is (> 1.01 (duration (pprogn (sleep 1) (sleep 1) (sleep 1))))))
