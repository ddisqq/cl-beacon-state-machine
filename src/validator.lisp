;;;; validator.lisp
;;;;
;;;; Validator structure and management for cl-beacon-state-machine
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:cl-beacon-state-machine)

;;; ============================================================================
;;; Validator Structure
;;; ============================================================================

(defstruct (validator
            (:constructor make-validator
                (&key pubkey withdrawal-credentials
                      (effective-balance 0) (slashed nil)
                      (activation-eligibility-epoch +far-future-epoch+)
                      (activation-epoch +far-future-epoch+)
                      (exit-epoch +far-future-epoch+)
                      (withdrawable-epoch +far-future-epoch+)))
            (:copier nil)
            (:predicate validator-p))
  "Validator record in the beacon state registry."
  (pubkey (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (48)))
  (withdrawal-credentials (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
                          :type (simple-array (unsigned-byte 8) (32)))
  (effective-balance 0 :type (integer 0))
  (slashed nil :type boolean)
  (activation-eligibility-epoch +far-future-epoch+ :type (integer 0))
  (activation-epoch +far-future-epoch+ :type (integer 0))
  (exit-epoch +far-future-epoch+ :type (integer 0))
  (withdrawable-epoch +far-future-epoch+ :type (integer 0)))

;;; ============================================================================
;;; Validator Lifecycle Predicates
;;; ============================================================================

(defun validator-is-active-p (validator epoch)
  "Check if validator is active at given epoch.
   Active means: activation_epoch <= epoch < exit_epoch"
  (and (<= (validator-activation-epoch validator) epoch)
       (< epoch (validator-exit-epoch validator))))

(defun validator-is-slashable-p (validator epoch)
  "Check if validator is slashable at given epoch.
   Slashable if not slashed AND active before withdrawable."
  (and (not (validator-slashed validator))
       (<= (validator-activation-epoch validator) epoch)
       (< epoch (validator-withdrawable-epoch validator))))

(defun validator-is-eligible-for-activation-queue-p (validator)
  "Check if validator is eligible for the activation queue."
  (= (validator-activation-eligibility-epoch validator) +far-future-epoch+))

(defun validator-is-exited-p (validator epoch)
  "Check if validator has exited."
  (<= (validator-exit-epoch validator) epoch))

(defun validator-is-withdrawable-p (validator epoch)
  "Check if validator is withdrawable."
  (and (validator-is-exited-p validator epoch)
       (<= (validator-withdrawable-epoch validator) epoch)))

;;; ============================================================================
;;; Validator Queries
;;; ============================================================================

(defun get-validator (state index)
  "Get validator by index."
  (let ((validators (beacon-state-validators state)))
    (if (< index (length validators))
        (aref validators index)
        (error "Validator not found: ~D" index))))

(defun get-validator-by-pubkey (state pubkey)
  "Get validator by public key. Returns (values validator index) or (values nil nil)."
  (loop for i from 0 below (length (beacon-state-validators state))
        for validator = (aref (beacon-state-validators state) i)
        when (equalp (validator-pubkey validator) pubkey)
          return (values validator i)
        finally (return (values nil nil))))

(defun get-active-validator-indices (state &optional epoch)
  "Get indices of all active validators at the given epoch."
  (let ((epoch (or epoch (get-current-epoch state))))
    (loop for i from 0 below (length (beacon-state-validators state))
          for v = (aref (beacon-state-validators state) i)
          when (validator-is-active-p v epoch)
            collect i)))

(defun get-active-validator-count (state &optional epoch)
  "Get count of active validators."
  (length (get-active-validator-indices state epoch)))

(defun get-total-balance (state indices)
  "Get sum of effective balances for specified validators.
   Returns at least EFFECTIVE_BALANCE_INCREMENT to avoid division by zero."
  (max +effective-balance-increment+
       (loop for idx in indices
             sum (validator-effective-balance (get-validator state idx)))))

(defun get-total-active-balance (state &optional epoch)
  "Get total effective balance of all active validators."
  (get-total-balance state (get-active-validator-indices state epoch)))

(defun get-validator-churn-limit (state)
  "Get the validator churn limit.
   Bounds how many validators can enter/exit per epoch."
  (let ((min-churn 4)
        (churn-quotient 65536)
        (active-count (get-active-validator-count state)))
    (max min-churn (floor active-count churn-quotient))))

;;; ============================================================================
;;; Committee and Proposer Selection
;;; ============================================================================

(defun compute-shuffled-index (index index-count seed)
  "Compute shuffled index using swap-or-not shuffle.
   Implements the eth2.0 committee shuffle algorithm."
  (when (zerop index-count)
    (return-from compute-shuffled-index index))
  (let ((current-index index))
    (dotimes (round +shuffle-round-count+ current-index)
      (let* ((pivot-input (make-array 33 :element-type '(unsigned-byte 8) :initial-element 0))
             (_ (progn
                  (replace pivot-input seed)
                  (setf (aref pivot-input 32) round)))
             (pivot-hash (sha256 pivot-input))
             (pivot (mod (bytes-to-integer (subseq pivot-hash 0 8)) index-count))
             (flip (mod (+ pivot index-count (- current-index)) index-count))
             (position (max current-index flip))
             (source-input (make-array 37 :element-type '(unsigned-byte 8) :initial-element 0)))
        (declare (ignore _))
        ;; Build source input
        (replace source-input seed)
        (setf (aref source-input 32) round)
        (let ((pos-div (floor position 256)))
          (loop for i from 0 below 4
                do (setf (aref source-input (+ 33 i))
                         (ldb (byte 8 (* i 8)) pos-div))))
        (let* ((source (sha256 source-input))
               (byte-val (aref source (floor (mod position 256) 8)))
               (bit-val (ldb (byte 1 (mod position 8)) byte-val)))
          (when (= bit-val 1)
            (setf current-index flip)))))))

(defun compute-proposer-index (state indices seed)
  "Compute proposer index from candidates using weighted random selection."
  (when (null indices)
    (return-from compute-proposer-index nil))
  (let ((n (length indices)))
    (loop for i from 0
          do (let* ((candidate-index (mod i n))
                    (candidate (nth candidate-index indices))
                    (hash-input (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0)))
               (replace hash-input seed)
               (loop for j from 0 below 8
                     do (setf (aref hash-input (+ 32 j))
                              (ldb (byte 8 (* j 8)) (floor i 32))))
               (let* ((hash (sha256 hash-input))
                      (random-byte (aref hash (mod i 32)))
                      (validator (get-validator state candidate))
                      (effective-balance (validator-effective-balance validator))
                      (threshold (floor (* effective-balance 256) +max-effective-balance+)))
                 (when (< random-byte threshold)
                   (return candidate)))))))

(defun get-beacon-proposer-index (state &optional slot)
  "Get the proposer index for a slot."
  (let* ((slot (or slot (beacon-state-slot state)))
         (epoch (compute-epoch-at-slot slot))
         (active-indices (get-active-validator-indices state epoch)))
    (when (null active-indices)
      (return-from get-beacon-proposer-index nil))
    (let* ((seed-input (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0))
           (randao-mix (get-randao-mix state epoch)))
      (replace seed-input randao-mix)
      (loop for i from 0 below 8
            do (setf (aref seed-input (+ 32 i))
                     (ldb (byte 8 (* i 8)) slot)))
      (let ((seed (sha256 seed-input)))
        (compute-proposer-index state active-indices seed)))))

(defun get-beacon-committee (state slot index)
  "Get the beacon committee for a slot and committee index."
  (let* ((epoch (compute-epoch-at-slot slot))
         (committees-per-slot (get-committee-count-per-slot state epoch))
         (active-indices (get-active-validator-indices state epoch))
         (index-count (length active-indices)))
    (when (zerop index-count)
      (return-from get-beacon-committee nil))
    (let* ((committee-count (* committees-per-slot +slots-per-epoch+))
           (slot-offset (mod slot +slots-per-epoch+))
           (committee-index (+ (* slot-offset committees-per-slot) index))
           (start (floor (* committee-index index-count) committee-count))
           (end (floor (* (1+ committee-index) index-count) committee-count))
           (seed (get-seed state epoch)))
      (loop for i from start below end
            collect (nth (compute-shuffled-index i index-count seed) active-indices)))))

(defun get-committee-count-per-slot (state epoch)
  "Get number of committees per slot for an epoch."
  (let ((active-count (get-active-validator-count state epoch)))
    (max 1
         (min +max-committees-per-slot+
              (floor (floor active-count +slots-per-epoch+)
                     +target-committee-size+)))))

(defun get-seed (state epoch)
  "Get seed for shuffling at an epoch."
  (let* ((mix-epoch (+ epoch +epochs-per-historical-vector+ (- +min-seed-lookahead+) -1))
         (input (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Epoch bytes (little-endian)
    (loop for i from 0 below 8
          do (setf (aref input i) (ldb (byte 8 (* i 8)) epoch)))
    ;; RANDAO mix
    (replace input (get-randao-mix state (mod mix-epoch +epochs-per-historical-vector+)) :start1 8)
    (sha256 input)))

;;; ============================================================================
;;; Validator State Mutators
;;; ============================================================================

(defun add-validator (state validator initial-balance)
  "Add a new validator to the registry. Returns the new validator index."
  (sb-thread:with-mutex ((beacon-state-lock state))
    (let ((index (length (beacon-state-validators state))))
      (vector-push-extend validator (beacon-state-validators state))
      (vector-push-extend initial-balance (beacon-state-balances state))
      (vector-push-extend 0 (beacon-state-previous-epoch-participation state))
      (vector-push-extend 0 (beacon-state-current-epoch-participation state))
      (vector-push-extend 0 (beacon-state-inactivity-scores state))
      index)))

(defun initiate-validator-exit (state index)
  "Initiate voluntary exit for a validator.
   Returns exit epoch, or NIL if already exiting."
  (sb-thread:with-mutex ((beacon-state-lock state))
    (let* ((validator (get-validator state index))
           (epoch (get-current-epoch state)))
      ;; Check not already exiting
      (when (< (validator-exit-epoch validator) +far-future-epoch+)
        (return-from initiate-validator-exit nil))
      ;; Compute exit epoch
      (let ((exit-queue-epoch (compute-activation-exit-epoch epoch))
            (exit-queue-churn 0))
        ;; Find exit queue epoch
        (loop for v across (beacon-state-validators state)
              when (= (validator-exit-epoch v) exit-queue-epoch)
                do (incf exit-queue-churn))
        ;; Bump if queue is full
        (when (>= exit-queue-churn (get-validator-churn-limit state))
          (incf exit-queue-epoch))
        ;; Set epochs
        (setf (validator-exit-epoch validator) exit-queue-epoch)
        (setf (validator-withdrawable-epoch validator)
              (+ exit-queue-epoch +epochs-per-slashings-vector+))
        exit-queue-epoch))))

(defun compute-activation-exit-epoch (epoch)
  "Compute the epoch at which a validator activation/exit takes effect."
  (+ epoch 1 +max-seed-lookahead+))

(defun slash-validator (state slashed-index &optional whistleblower-index)
  "Slash a validator for byzantine behavior. Returns amount slashed."
  (sb-thread:with-mutex ((beacon-state-lock state))
    (let* ((epoch (get-current-epoch state))
           (validator (get-validator state slashed-index))
           (effective-balance (validator-effective-balance validator)))
      ;; Check not already slashed
      (when (validator-slashed validator)
        (return-from slash-validator 0))
      ;; Set slashed
      (setf (validator-slashed validator) t)
      ;; Initiate exit if not already exiting
      (when (= (validator-exit-epoch validator) +far-future-epoch+)
        (let ((exit-epoch (compute-activation-exit-epoch epoch)))
          (setf (validator-exit-epoch validator) exit-epoch)
          (setf (validator-withdrawable-epoch validator)
                (+ exit-epoch +epochs-per-slashings-vector+))))
      ;; Set withdrawable epoch
      (setf (validator-withdrawable-epoch validator)
            (max (validator-withdrawable-epoch validator)
                 (+ epoch +epochs-per-slashings-vector+)))
      ;; Update slashings accumulator
      (let ((slashing-index (mod epoch +epochs-per-slashings-vector+)))
        (incf (aref (beacon-state-slashings state) slashing-index)
              effective-balance))
      ;; Apply initial penalty
      (let ((penalty (floor effective-balance +min-slashing-penalty-quotient+)))
        (decrease-balance state slashed-index penalty))
      ;; Reward whistleblower/proposer
      (let* ((proposer-index (get-beacon-proposer-index state))
             (whistleblower-index (or whistleblower-index proposer-index))
             (whistleblower-reward (floor effective-balance +whistleblower-reward-quotient+))
             (proposer-reward (floor (* whistleblower-reward +proposer-weight+)
                                      +weight-denominator+)))
        (increase-balance state proposer-index proposer-reward)
        (increase-balance state whistleblower-index
                          (- whistleblower-reward proposer-reward)))
      effective-balance)))

;;; ============================================================================
;;; Balance Operations
;;; ============================================================================

(defun increase-balance (state index delta)
  "Increase a validator's balance."
  (let ((balances (beacon-state-balances state)))
    (when (< index (length balances))
      (incf (aref balances index) delta)
      (aref balances index))))

(defun decrease-balance (state index delta)
  "Decrease a validator's balance (floor at 0)."
  (let ((balances (beacon-state-balances state)))
    (when (< index (length balances))
      (let ((current (aref balances index)))
        (setf (aref balances index)
              (if (>= current delta)
                  (- current delta)
                  0))
        (aref balances index)))))

(defun get-balance (state index)
  "Get balance for a validator."
  (let ((balances (beacon-state-balances state)))
    (if (< index (length balances))
        (aref balances index)
        (error "Validator not found: ~D" index))))

(defun get-effective-balance (state index)
  "Get effective balance for a validator."
  (validator-effective-balance (get-validator state index)))

;;; ============================================================================
;;; Participation Flag Operations
;;; ============================================================================

(defun set-participation-flag (state index flag-index epoch-type)
  "Set a participation flag for a validator.
   FLAGS: 0=source, 1=target, 2=head
   EPOCH-TYPE: :previous or :current"
  (let ((participation (if (eq epoch-type :previous)
                           (beacon-state-previous-epoch-participation state)
                           (beacon-state-current-epoch-participation state))))
    (when (< index (length participation))
      (setf (aref participation index)
            (logior (aref participation index)
                    (ash 1 flag-index)))
      (aref participation index))))

(defun rotate-participation (state)
  "Rotate participation arrays at epoch boundary.
   Moves current to previous and zeros current."
  ;; Copy current to previous
  (loop for i from 0 below (length (beacon-state-current-epoch-participation state))
        do (setf (aref (beacon-state-previous-epoch-participation state) i)
                 (aref (beacon-state-current-epoch-participation state) i)))
  ;; Zero current
  (loop for i from 0 below (length (beacon-state-current-epoch-participation state))
        do (setf (aref (beacon-state-current-epoch-participation state) i) 0))
  t)

(defun get-unslashed-participating-indices (state flag-index epoch)
  "Get indices of unslashed validators with a participation flag set."
  (let ((participation (if (= epoch (get-previous-epoch state))
                           (beacon-state-previous-epoch-participation state)
                           (beacon-state-current-epoch-participation state))))
    (loop for i from 0 below (length participation)
          for validator = (aref (beacon-state-validators state) i)
          for flags = (aref participation i)
          when (and (validator-is-active-p validator epoch)
                    (not (validator-slashed validator))
                    (logbitp flag-index flags))
            collect i)))
