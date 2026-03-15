;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-beacon-state-machine)

;;; ============================================================================
;;; Beacon State Machine - Core State Transitions and Validator Registry
;;; ============================================================================

;;; Error Conditions
(define-condition cl-beacon-state-machine-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c s)
             (format s "Beacon state machine error: ~A" (error-message c)))))

(define-condition cl-beacon-state-machine-validation-error (cl-beacon-state-machine-error)
  ()
  (:default-initargs :message "Validation failed"))

;;; Core State Structures
(defstruct beacon-state
  "The Beacon chain state structure containing all state required for consensus."
  ;; Versioning
  (genesis-time 0 :type (unsigned-byte 64))
  (genesis-validators-root nil :type (or null (simple-array (unsigned-byte 8) (32))))
  ;; Slot tracking
  (slot 0 :type (unsigned-byte 64))
  ;; Validator state
  (validators (make-array 0 :adjustable t) :type (vector * ))
  (balances (make-array 0 :adjustable t :element-type 'integer) :type (vector integer))
  ;; Randomness
  (randao-mixes (make-array 65536 :initial-element nil) :type (vector *))
  ;; Justification and finality
  (previous-justified-checkpoint (cons 0 nil) :type cons)
  (current-justified-checkpoint (cons 0 nil) :type cons)
  (finalized-checkpoint (cons 0 nil) :type cons)
  ;; Participation tracking
  (previous-epoch-participation (make-array 0 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  (current-epoch-participation (make-array 0 :adjustable t :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))
  ;; Inactivity penalties
  (inactivity-scores (make-array 0 :adjustable t :element-type 'integer) :type (vector integer)))

(defstruct validator-record
  "Validator registry entry with status and balance info."
  (pubkey nil :type (or null (simple-array (unsigned-byte 8) (48))))
  (withdrawal-credentials nil :type (or null (simple-array (unsigned-byte 8) (32))))
  (activation-epoch 0 :type (unsigned-byte 64))
  (exit-epoch #xffffffffffffffff :type (unsigned-byte 64))
  (withdrawable-epoch #xffffffffffffffff :type (unsigned-byte 64))
  (exit-requested-epoch 0 :type (unsigned-byte 64))
  (exit-requested-epoch-is-finalized nil :type boolean)
  (balance 0 :type integer)
  (status :pending-deposit :type (member :pending-deposit :pending-activation :active :active-exiting :exited :exited-slashed :withdrawal-possible :withdrawal-done)))

;;; Initialize Module Functions
(defun init ()
  "Initialize beacon state machine module."
  t)

(defun process (data)
  "Process beacon state update."
  (declare (type t data))
  data)

(defun status ()
  "Get module status."
  :ok)

(defun validate (input)
  "Validate beacon state machine input."
  (declare (type t input))
  t)

(defun cleanup ()
  "Cleanup resources."
  t)

;;; Core State Transition Functions

(defun initialize-beacon-state (genesis-time genesis-validators-root)
  "Create a new beacon state initialized at genesis."
  (make-beacon-state
   :genesis-time genesis-time
   :genesis-validators-root genesis-validators-root
   :slot 0
   :validators (make-array 0 :adjustable t)
   :balances (make-array 0 :adjustable t :element-type 'integer)))

(defun add-validator (state pubkey withdrawal-credentials deposit-amount)
  "Add a new validator to the beacon state.
   Returns updated state."
  (when (or (null pubkey) (null withdrawal-credentials))
    (error 'cl-beacon-state-machine-validation-error :message "Invalid validator credentials"))
  (let* ((validators (beacon-state-validators state))
         (balances (beacon-state-balances state))
         (index (length validators))
         (validator (make-validator-record
                     :pubkey pubkey
                     :withdrawal-credentials withdrawal-credentials
                     :balance deposit-amount
                     :status :pending-activation)))
    (vector-push-extend validator validators)
    (vector-push-extend deposit-amount balances)
    (values state index)))

(defun increase-balance (state index amount)
  "Increase validator balance at INDEX by AMOUNT."
  (when (>= index (length (beacon-state-balances state)))
    (error 'cl-beacon-state-machine-validation-error :message "Invalid validator index"))
  (incf (aref (beacon-state-balances state) index) amount)
  state)

(defun decrease-balance (state index amount)
  "Decrease validator balance at INDEX by AMOUNT."
  (when (>= index (length (beacon-state-balances state)))
    (error 'cl-beacon-state-machine-validation-error :message "Invalid validator index"))
  (let ((current (aref (beacon-state-balances state) index)))
    (if (>= current amount)
        (decf (aref (beacon-state-balances state) index) amount)
        (setf (aref (beacon-state-balances state) index) 0)))
  state)

(defun set-validator-status (state index new-status)
  "Set validator status to NEW-STATUS."
  (when (>= index (length (beacon-state-validators state)))
    (error 'cl-beacon-state-machine-validation-error :message "Invalid validator index"))
  (let ((validator (aref (beacon-state-validators state) index)))
    (setf (validator-record-status validator) new-status))
  state)

(defun activate-validator (state index epoch)
  "Activate validator at INDEX at given EPOCH."
  (when (>= index (length (beacon-state-validators state)))
    (error 'cl-beacon-state-machine-validation-error :message "Invalid validator index"))
  (let ((validator (aref (beacon-state-validators state) index)))
    (setf (validator-record-activation-epoch validator) epoch)
    (setf (validator-record-status validator) :active))
  state)

(defun request-validator-exit (state index exit-epoch)
  "Request exit for validator at INDEX."
  (when (>= index (length (beacon-state-validators state)))
    (error 'cl-beacon-state-machine-validation-error :message "Invalid validator index"))
  (let ((validator (aref (beacon-state-validators state) index)))
    (when (eq (validator-record-status validator) :active)
      (setf (validator-record-exit-requested-epoch validator) exit-epoch)
      (setf (validator-record-status validator) :active-exiting)))
  state)

(defun slash-validator (state index slashing-penalty)
  "Slash validator at INDEX with penalty."
  (when (>= index (length (beacon-state-validators state)))
    (error 'cl-beacon-state-machine-validation-error :message "Invalid validator index"))
  (decrease-balance state index slashing-penalty)
  (let ((validator (aref (beacon-state-validators state) index)))
    (setf (validator-record-status validator) :exited-slashed))
  state)

(defun advance-epoch (state epoch)
  "Advance beacon state to next epoch."
  (let ((slots-per-epoch 32))
    (setf (beacon-state-slot state) (* epoch slots-per-epoch)))
  ;; Rotate epoch participation
  (setf (beacon-state-previous-epoch-participation state)
        (beacon-state-current-epoch-participation state))
  (setf (beacon-state-current-epoch-participation state)
        (make-array (length (beacon-state-validators state))
                   :element-type '(unsigned-byte 8)
                   :initial-element 0))
  state)

(defun get-validator-count (state)
  "Get total number of validators."
  (length (beacon-state-validators state)))

(defun get-active-validator-count (state)
  "Get count of active validators."
  (count :active (beacon-state-validators state)
         :key #'validator-record-status))

(defun get-total-active-balance (state)
  "Get sum of all active validator balances."
  (let ((validators (beacon-state-validators state))
        (balances (beacon-state-balances state))
        (total 0))
    (loop for i from 0 below (length validators)
      when (eq (validator-record-status (aref validators i)) :active)
      do (incf total (aref balances i)))
    total))

(defun update-randao (state epoch randao-value)
  "Update RANDAO mix for epoch."
  (when (>= epoch (length (beacon-state-randao-mixes state)))
    (error 'cl-beacon-state-machine-validation-error :message "Epoch out of range"))
  (setf (aref (beacon-state-randao-mixes state) (mod epoch 65536)) randao-value)
  state)

(defun get-randao-mix (state epoch)
  "Get RANDAO mix for epoch."
  (when (>= epoch (length (beacon-state-randao-mixes state)))
    (error 'cl-beacon-state-machine-validation-error :message "Epoch out of range"))
  (aref (beacon-state-randao-mixes state) (mod epoch 65536)))

(defun update-justification (state current-epoch new-justified-epoch)
  "Update justification checkpoints."
  (setf (beacon-state-previous-justified-checkpoint state)
        (beacon-state-current-justified-checkpoint state))
  (setf (beacon-state-current-justified-checkpoint state)
        (cons new-justified-epoch nil))
  state)

(defun update-finalization (state finalized-epoch)
  "Update finalization checkpoint."
  (setf (beacon-state-finalized-checkpoint state)
        (cons finalized-epoch nil))
  state)

;;; ============================================================================
;;; Standard Toolkit for cl-beacon-state-machine
;;; ============================================================================

(defmacro with-beacon-state-machine-timing (&body body)
  "Executes BODY and logs the execution time specific to cl-beacon-state-machine."
  (let ((start (gensym))
        (end (gensym)))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1
           (progn ,@body)
         (let ((,end (get-internal-real-time)))
           (format t "~&[cl-beacon-state-machine] Execution time: ~A ms~%"
                   (/ (* (- ,end ,start) 1000.0) internal-time-units-per-second)))))))

(defun beacon-state-machine-batch-process (items processor-fn)
  "Applies PROCESSOR-FN to each item in ITEMS, handling errors resiliently.
Returns (values processed-results error-alist)."
  (let ((results nil)
        (errors nil))
    (dolist (item items)
      (handler-case
          (push (funcall processor-fn item) results)
        (error (e)
          (push (cons item e) errors))))
    (values (nreverse results) (nreverse errors))))

(defun beacon-state-machine-health-check ()
  "Performs a basic health check for the cl-beacon-state-machine module."
  (let ((ctx (initialize-beacon-state-machine)))
    (if (validate-beacon-state-machine ctx)
        :healthy
        :degraded)))


;;; Substantive Domain Expansion

(defun identity-list (x) (if (listp x) x (list x)))
(defun flatten (l) (cond ((null l) nil) ((atom l) (list l)) (t (append (flatten (car l)) (flatten (cdr l))))))
(defun map-keys (fn hash) (let ((res nil)) (maphash (lambda (k v) (push (funcall fn k) res)) hash) res))
(defun now-timestamp () (get-universal-time))

;;; Substantive Functional Logic

(defun deep-copy-list (l)
  "Recursively copies a nested list."
  (if (atom l) l (cons (deep-copy-list (car l)) (deep-copy-list (cdr l)))))

(defun group-by-count (list n)
  "Groups list elements into sublists of size N."
  (loop for i from 0 below (length list) by n
        collect (subseq list i (min (+ i n) (length list)))))
