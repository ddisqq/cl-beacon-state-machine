;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

;;;; test-beacon.lisp
;;;;
;;;; Test suite for cl-beacon-state-machine
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: Apache-2.0

(in-package #:cl-beacon-state-machine.test)

;;; ============================================================================
;;; Test Infrastructure
;;; ============================================================================

(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defmacro define-test (name &body body)
  "Define a test case."
  `(defun ,name ()
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (incf *test-passed*)
           (format t "  PASS: ~A~%" ',name)
           t)
       (error (e)
         (incf *test-failed*)
         (format t "  FAIL: ~A - ~A~%" ',name e)
         nil))))

(defmacro assert-true (form &optional message)
  "Assert that FORM evaluates to true."
  `(unless ,form
     (error "Assertion failed~@[: ~A~]" ,message)))

(defmacro assert-equal (expected actual &optional message)
  "Assert that EXPECTED equals ACTUAL."
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equal exp act)
       (error "Expected ~S but got ~S~@[: ~A~]" exp act ,message))))

(defmacro assert-equalp (expected actual &optional message)
  "Assert that EXPECTED equals ACTUAL using EQUALP."
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equalp exp act)
       (error "Expected ~S but got ~S~@[: ~A~]" exp act ,message))))

(defmacro assert-error (form &optional message)
  "Assert that FORM signals an error."
  `(handler-case
       (progn ,form
              (error "Expected error but none signaled~@[: ~A~]" ,message))
     (error () t)))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun make-test-pubkey (n)
  "Create a test public key."
  (let ((key (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref key 0) (mod n 256))
    (setf (aref key 1) (mod (ash n -8) 256))
    key))

(defun make-test-withdrawal-creds (n)
  "Create test withdrawal credentials."
  (let ((creds (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref creds 0) #x01)  ; ETH1 withdrawal prefix
    (setf (aref creds 31) (mod n 256))
    creds))

(defun make-test-state (&key (validator-count 4))
  "Create a test beacon state with validators."
  (let ((validators nil)
        (genesis-time (get-universal-time)))
    (dotimes (i validator-count)
      (push (list (make-test-pubkey i)
                  (make-test-withdrawal-creds i)
                  +max-effective-balance+)
            validators))
    (create-genesis-state (nreverse validators) genesis-time)))

;;; ============================================================================
;;; SHA-256 Tests
;;; ============================================================================

(define-test test-sha256-empty
  "Test SHA-256 of empty input."
  (let* ((data (make-array 0 :element-type '(unsigned-byte 8)))
         (hash (sha256 data)))
    (assert-equal 32 (length hash))
    ;; SHA256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    (assert-equal #xe3 (aref hash 0))
    (assert-equal #xb0 (aref hash 1))))

(define-test test-sha256-abc
  "Test SHA-256 of 'abc'."
  (let* ((data (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(97 98 99)))  ; "abc"
         (hash (sha256 data)))
    ;; SHA256("abc") = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
    (assert-equal #xba (aref hash 0))
    (assert-equal #x78 (aref hash 1))
    (assert-equal #x16 (aref hash 2))))

;;; ============================================================================
;;; Utility Tests
;;; ============================================================================

(define-test test-integer-square-root
  "Test integer square root computation."
  (assert-equal 0 (integer-square-root 0))
  (assert-equal 1 (integer-square-root 1))
  (assert-equal 3 (integer-square-root 9))
  (assert-equal 3 (integer-square-root 10))
  (assert-equal 10 (integer-square-root 100))
  (assert-equal 31 (integer-square-root 1000)))

(define-test test-bytes-integer-roundtrip
  "Test bytes <-> integer conversion."
  (let* ((n 12345678)
         (bytes (integer-to-bytes n 8))
         (result (bytes-to-integer bytes)))
    (assert-equal n result)))

(define-test test-xor-bytes
  "Test XOR of byte arrays."
  (let* ((a (make-array 4 :element-type '(unsigned-byte 8)
                        :initial-contents '(#xFF #x00 #xAA #x55)))
         (b (make-array 4 :element-type '(unsigned-byte 8)
                        :initial-contents '(#x0F #xF0 #x55 #xAA)))
         (result (xor-bytes a b)))
    (assert-equal #xF0 (aref result 0))
    (assert-equal #xF0 (aref result 1))
    (assert-equal #xFF (aref result 2))
    (assert-equal #xFF (aref result 3))))

;;; ============================================================================
;;; State Tests
;;; ============================================================================

(define-test test-make-beacon-state
  "Test beacon state creation."
  (let ((state (make-beacon-state :genesis-time 1000)))
    (assert-true (beacon-state-p state))
    (assert-equal 1000 (beacon-state-genesis-time state))
    (assert-equal 0 (beacon-state-slot state))))

(define-test test-compute-epoch-at-slot
  "Test epoch computation from slot."
  (assert-equal 0 (compute-epoch-at-slot 0))
  (assert-equal 0 (compute-epoch-at-slot 31))
  (assert-equal 1 (compute-epoch-at-slot 32))
  (assert-equal 1 (compute-epoch-at-slot 63))
  (assert-equal 2 (compute-epoch-at-slot 64)))

(define-test test-compute-start-slot-at-epoch
  "Test start slot computation from epoch."
  (assert-equal 0 (compute-start-slot-at-epoch 0))
  (assert-equal 32 (compute-start-slot-at-epoch 1))
  (assert-equal 64 (compute-start-slot-at-epoch 2)))

;;; ============================================================================
;;; Validator Tests
;;; ============================================================================

(define-test test-make-validator
  "Test validator creation."
  (let ((v (make-validator :pubkey (make-test-pubkey 1)
                           :effective-balance +max-effective-balance+
                           :activation-epoch 0)))
    (assert-true (validator-p v))
    (assert-equal +max-effective-balance+ (validator-effective-balance v))
    (assert-equal 0 (validator-activation-epoch v))))

(define-test test-validator-is-active
  "Test validator active check."
  (let ((v (make-validator :activation-epoch 0 :exit-epoch 100)))
    (assert-true (validator-is-active-p v 0))
    (assert-true (validator-is-active-p v 50))
    (assert-true (validator-is-active-p v 99))
    (assert-true (not (validator-is-active-p v 100)))
    (assert-true (not (validator-is-active-p v 200)))))

(define-test test-add-validator
  "Test adding validator to state."
  (let ((state (make-beacon-state))
        (v (make-validator :pubkey (make-test-pubkey 1)
                           :effective-balance +max-effective-balance+
                           :activation-epoch 0)))
    (let ((idx (add-validator state v +max-effective-balance+)))
      (assert-equal 0 idx)
      (assert-equal 1 (length (beacon-state-validators state)))
      (assert-equal +max-effective-balance+ (get-balance state 0)))))

;;; ============================================================================
;;; Balance Tests
;;; ============================================================================

(define-test test-increase-decrease-balance
  "Test balance modifications."
  (let ((state (make-test-state :validator-count 1)))
    (let ((initial (get-balance state 0)))
      (increase-balance state 0 1000)
      (assert-equal (+ initial 1000) (get-balance state 0))
      (decrease-balance state 0 500)
      (assert-equal (+ initial 500) (get-balance state 0)))))

(define-test test-decrease-balance-floor
  "Test balance decrease floors at 0."
  (let ((state (make-test-state :validator-count 1)))
    (decrease-balance state 0 (+ (get-balance state 0) 1000000))
    (assert-equal 0 (get-balance state 0))))

;;; ============================================================================
;;; Slot Processing Tests
;;; ============================================================================

(define-test test-process-slots
  "Test slot advancement."
  (let ((state (make-test-state)))
    (assert-equal 0 (beacon-state-slot state))
    (process-slots state 10)
    (assert-equal 10 (beacon-state-slot state))))

(define-test test-process-slots-epoch-boundary
  "Test epoch processing at boundary."
  (let ((state (make-test-state :validator-count 4)))
    (process-slots state 32)  ; First epoch boundary
    (assert-equal 32 (beacon-state-slot state))
    (assert-equal 1 (get-current-epoch state))))

;;; ============================================================================
;;; Committee Tests
;;; ============================================================================

(define-test test-get-active-validator-indices
  "Test active validator index retrieval."
  (let ((state (make-test-state :validator-count 4)))
    (let ((indices (get-active-validator-indices state)))
      (assert-equal 4 (length indices))
      (assert-true (member 0 indices))
      (assert-true (member 3 indices)))))

(define-test test-get-beacon-proposer
  "Test proposer selection."
  (let ((state (make-test-state :validator-count 4)))
    (let ((proposer (get-beacon-proposer-index state)))
      (assert-true (and (integerp proposer)
                        (>= proposer 0)
                        (< proposer 4))))))

;;; ============================================================================
;;; Checkpoint Tests
;;; ============================================================================

(define-test test-checkpoint-equal
  "Test checkpoint equality."
  (let* ((root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
         (c1 (make-checkpoint :epoch 5 :root root))
         (c2 (make-checkpoint :epoch 5 :root (copy-seq root)))
         (c3 (make-checkpoint :epoch 6 :root root)))
    (assert-true (checkpoint-equal-p c1 c2))
    (assert-true (not (checkpoint-equal-p c1 c3)))))

;;; ============================================================================
;;; Attestation Tests
;;; ============================================================================

(define-test test-make-attestation-data
  "Test attestation data creation."
  (let* ((source (make-checkpoint :epoch 0))
         (target (make-checkpoint :epoch 1))
         (data (make-attestation-data :slot 32
                                      :index 0
                                      :source source
                                      :target target)))
    (assert-true (attestation-data-p data))
    (assert-equal 32 (attestation-data-slot data))
    (assert-equal 0 (attestation-data-index data))))

;;; ============================================================================
;;; Slashing Tests
;;; ============================================================================

(define-test test-slash-validator
  "Test validator slashing."
  (let ((state (make-test-state :validator-count 4)))
    (let ((slashed-amount (slash-validator state 0)))
      (assert-true (> slashed-amount 0))
      (assert-true (validator-slashed (get-validator state 0))))))

(define-test test-slash-validator-once
  "Test that slashing twice returns 0."
  (let ((state (make-test-state :validator-count 4)))
    (slash-validator state 0)
    (let ((second-slash (slash-validator state 0)))
      (assert-equal 0 second-slash))))

;;; ============================================================================
;;; Exit Tests
;;; ============================================================================

(define-test test-initiate-exit
  "Test voluntary exit initiation."
  (let ((state (make-test-state :validator-count 4)))
    ;; Process enough slots to allow exit
    (process-slots state (* +shard-committee-period+ +slots-per-epoch+))
    (let ((exit-epoch (initiate-validator-exit state 0)))
      (assert-true (integerp exit-epoch))
      (assert-true (< (validator-exit-epoch (get-validator state 0))
                      +far-future-epoch+)))))

;;; ============================================================================
;;; Genesis Tests
;;; ============================================================================

(define-test test-create-genesis-state
  "Test genesis state creation."
  (let ((validators (list (list (make-test-pubkey 0)
                                (make-test-withdrawal-creds 0)
                                +max-effective-balance+)
                          (list (make-test-pubkey 1)
                                (make-test-withdrawal-creds 1)
                                +max-effective-balance+))))
    (let ((state (create-genesis-state validators 1000000)))
      (assert-true (beacon-state-p state))
      (assert-equal 2 (length (beacon-state-validators state)))
      (assert-equal 0 (beacon-state-slot state)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all tests and report results."
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)

  (format t "~%Running cl-beacon-state-machine tests...~%~%")

  ;; SHA-256 tests
  (format t "SHA-256 Tests:~%")
  (test-sha256-empty)
  (test-sha256-abc)

  ;; Utility tests
  (format t "~%Utility Tests:~%")
  (test-integer-square-root)
  (test-bytes-integer-roundtrip)
  (test-xor-bytes)

  ;; State tests
  (format t "~%State Tests:~%")
  (test-make-beacon-state)
  (test-compute-epoch-at-slot)
  (test-compute-start-slot-at-epoch)

  ;; Validator tests
  (format t "~%Validator Tests:~%")
  (test-make-validator)
  (test-validator-is-active)
  (test-add-validator)

  ;; Balance tests
  (format t "~%Balance Tests:~%")
  (test-increase-decrease-balance)
  (test-decrease-balance-floor)

  ;; Slot processing tests
  (format t "~%Slot Processing Tests:~%")
  (test-process-slots)
  (test-process-slots-epoch-boundary)

  ;; Committee tests
  (format t "~%Committee Tests:~%")
  (test-get-active-validator-indices)
  (test-get-beacon-proposer)

  ;; Checkpoint tests
  (format t "~%Checkpoint Tests:~%")
  (test-checkpoint-equal)

  ;; Attestation tests
  (format t "~%Attestation Tests:~%")
  (test-make-attestation-data)

  ;; Slashing tests
  (format t "~%Slashing Tests:~%")
  (test-slash-validator)
  (test-slash-validator-once)

  ;; Exit tests
  (format t "~%Exit Tests:~%")
  (test-initiate-exit)

  ;; Genesis tests
  (format t "~%Genesis Tests:~%")
  (test-create-genesis-state)

  ;; Summary
  (format t "~%========================================~%")
  (format t "Tests run: ~D~%" *test-count*)
  (format t "Passed:    ~D~%" *test-passed*)
  (format t "Failed:    ~D~%" *test-failed*)
  (format t "========================================~%")

  (if (zerop *test-failed*)
      (format t "~%All tests passed!~%")
      (format t "~%Some tests failed.~%"))

  (zerop *test-failed*))
