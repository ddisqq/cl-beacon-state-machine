;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(defpackage #:cl-beacon-state-machine.test
  (:use #:cl #:cl-beacon-state-machine)
  (:export #:run-tests))

(in-package #:cl-beacon-state-machine.test)

(defun run-tests ()
  (format t "Running professional test suite for cl-beacon-state-machine...~%")
  (assert (initialize-beacon-state-machine))
  (format t "Tests passed!~%")
  t)
