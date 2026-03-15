;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

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
  :author "Park Ian Co"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :depends-on ()  ; Pure Common Lisp - no external dependencies
  :components
  ((:module "src"
                :components ((:file "package")
                             (:file "conditions" :depends-on ("package"))
                             (:file "types" :depends-on ("package"))
                             (:file "util" :depends-on ("package"))
                             (:file "state" :depends-on ("package" "conditions" "types"))
                             (:file "validator" :depends-on ("package" "conditions" "types" "state"))
                             (:file "attestation" :depends-on ("package" "conditions" "types" "state" "validator"))
                             (:file "epoch" :depends-on ("package" "conditions" "types" "state" "validator"))
                             (:file "beacon" :depends-on ("package" "conditions" "types" "state" "validator" "epoch" "attestation"))
                             (:file "cl-beacon-state-machine" :depends-on ("package" "conditions" "types" "beacon" "validator" "state")))))
  :in-order-to ((asdf:test-op (test-op #:cl-beacon-state-machine/test))))

(asdf:defsystem #:cl-beacon-state-machine/test
  :description "Tests for cl-beacon-state-machine"
  :author "Park Ian Co"
  :license "Apache-2.0"
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
