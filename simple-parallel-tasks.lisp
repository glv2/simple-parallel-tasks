;;; simple-parallel-tasks
;;; Copyright 2019 Guillaume Le Vaillant
;;; This library is free software released under the GNU GPL-3 license.

(defpackage :simple-parallel-tasks
  (:use :cl :chanl)
  (:export #:plist #:pvalues #:plet))

(in-package :simple-parallel-tasks)


(defmacro plist (&rest forms)
  "Evaluate FORMS in parallel and return the results in a list."
  (let* ((channels (loop repeat (length forms)
                         collect (gensym)))
         (bindings (mapcar (lambda (channel)
                             `(,channel (make-instance 'bounded-channel)))
                           channels))
         (start-tasks (mapcar (lambda (channel form)
                                `(pcall (lambda ()
                                          (send ,channel ,form))))
                              channels
                              forms))
         (results (mapcar (lambda (channel)
                            `(recv ,channel))
                          channels)))
    `(let ,bindings
       ,@start-tasks
       (list ,@results))))

(defmacro pvalues (&rest forms)
  "Evaluate FORMS in parallel and return the results as multiple values."
  (let* ((channels (loop repeat (length forms)
                         collect (gensym)))
         (bindings (mapcar (lambda (channel)
                             `(,channel (make-instance 'bounded-channel)))
                           channels))
         (start-tasks (mapcar (lambda (channel form)
                                `(pcall (lambda ()
                                          (send ,channel ,form))))
                              channels
                              forms))
         (results (mapcar (lambda (channel)
                            `(recv ,channel))
                          channels)))
    `(let ,bindings
       ,@start-tasks
       (values ,@results))))

(defmacro plet (bindings &body body)
  "Like LET, but the BINDINGS are evaluated in parallel."
  (let* ((channels (loop repeat (length bindings)
                         collect (gensym)))
         (vars (mapcar (lambda (binding)
                         (if (listp binding) (car binding) binding))
                       bindings))
         (forms (mapcar (lambda (binding)
                          (if (listp binding) (cadr binding) nil))
                        bindings))
         (chan-bindings (mapcar (lambda (channel)
                                  `(,channel (make-instance 'bounded-channel)))
                                channels))
         (start-tasks (mapcar (lambda (channel form)
                                `(pcall (lambda ()
                                          (send ,channel ,form))))
                              channels
                              forms))
         (var-bindings (mapcar (lambda (var channel)
                                 `(,var (recv ,channel)))
                               vars
                               channels)))
    `(let ,chan-bindings
       ,@start-tasks
       (let ,var-bindings
         ,@body))))
