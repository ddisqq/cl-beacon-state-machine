;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

(load "cl-beacon-state-machine.asd")
(handler-case
  (progn
    (asdf:test-system :cl-beacon-state-machine/test)
    (format t "PASS~%"))
  (error (e)
    (format t "FAIL~%")))
(quit)
