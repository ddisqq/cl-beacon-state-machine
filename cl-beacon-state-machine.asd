;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; cl-beacon-state-machine.asd
;;;;
;;;; ASDF system definition for cl-beacon-state-machine
;;;; A standalone PoS beacon chain state machine implementation
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(asdf:defsystem #:cl-beacon-state-machine
  :description "Proof-of-Stake beacon chain state machine with epoch processing, validators, and attestations"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "0.1.0"
  :serial t
  :depends-on ()  ; Pure Common Lisp - no external dependencies
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "state")
     (:file "validator")
     (:file "attestation")
     (:file "epoch")
     (:file "beacon"))))
  :in-order-to ((test-op (test-op #:cl-beacon-state-machine/test))))

(asdf:defsystem #:cl-beacon-state-machine/test
  :description "Tests for cl-beacon-state-machine"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :depends-on (#:cl-beacon-state-machine)
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "test-beacon"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-beacon-state-machine.test :run-tests)))
               (unless result
                 (error "Tests failed")))))
