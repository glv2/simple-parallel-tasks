;;; simple-parallel-tasks
;;; Copyright 2019 Guillaume Le Vaillant
;;; This library is free software released under the GNU GPL-3 license.

(defsystem "simple-parallel-tasks"
  :name "simple-parallel-tasks"
  :description "Evaluate forms in parallel"
  :version "1.0"
  :author "Guillaume Le Vaillant"
  :license "GPL-3"
  :depends-on ("chanl")
  :in-order-to ((test-op (test-op "simple-parallel-tasks-tests")))
  :components ((:file "simple-parallel-tasks")))
