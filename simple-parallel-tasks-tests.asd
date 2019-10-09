;;; simple-parallel-tasks
;;; Copyright 2019 Guillaume Le Vaillant
;;; This library is free software released under the GNU GPL-3 license.

(defsystem "simple-parallel-tasks-tests"
  :name "simple-parallel-tasks-tests"
  :description "Tests for simple-parallel-tasks"
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("fiveam" "simple-parallel-tasks")
  :in-order-to ((test-op (load-op "simple-parallel-tasks-tests")))
  :perform (test-op (o s)
             (let ((tests (uiop:find-symbol* 'simple-parallel-tasks-tests
                                             :simple-parallel-tasks-tests)))
               (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:file "simple-parallel-tasks-tests")))
