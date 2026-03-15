;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

;;;; beacon.lisp
;;;;
;;;; Top-level state transition functions for cl-beacon-state-machine
;;;; Implements process_slots, process_block, and state_transition
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: Apache-2.0

(in-package #:cl-beacon-state-machine)

;;; ============================================================================
;;; Block Header Structure
;;; ============================================================================

(defstruct (beacon-block-header
            (:constructor make-beacon-block-header
                (&key (slot 0) (proposer-index 0) parent-root state-root body-root))
            (:copier nil)
            (:predicate beacon-block-header-p))
  "Beacon block header containing essential block metadata."
  (slot 0 :type (integer 0))
  (proposer-index 0 :type (integer 0))
  (parent-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
               :type (simple-array (unsigned-byte 8) (32)))
  (state-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
              :type (simple-array (unsigned-byte 8) (32)))
  (body-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
             :type (simple-array (unsigned-byte 8) (32))))

;;; ============================================================================
;;; Block Body Structure
;;; ============================================================================

(defstruct (beacon-block-body
            (:constructor make-beacon-block-body
                (&key randao-reveal eth1-data graffiti
                      proposer-slashings attester-slashings
                      attestations deposits voluntary-exits))
            (:copier nil)
            (:predicate beacon-block-body-p))
  "Beacon block body containing all operations."
  ;; Consensus
  (randao-reveal (make-array 96 :element-type '(unsigned-byte 8) :initial-element 0)
                 :type (simple-array (unsigned-byte 8) (96)))
  (eth1-data nil :type (or null eth1-data))
  (graffiti (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
            :type (simple-array (unsigned-byte 8) (32)))
  ;; Operations
  (proposer-slashings nil :type list)
  (attester-slashings nil :type list)
  (attestations nil :type list)
  (deposits nil :type list)
  (voluntary-exits nil :type list))

;;; ============================================================================
;;; Beacon Block Structure
;;; ============================================================================

(defstruct (beacon-block
            (:constructor make-beacon-block
                (&key (slot 0) (proposer-index 0) parent-root state-root body))
            (:copier nil)
            (:predicate beacon-block-p))
  "Complete beacon block."
  (slot 0 :type (integer 0))
  (proposer-index 0 :type (integer 0))
  (parent-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
               :type (simple-array (unsigned-byte 8) (32)))
  (state-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
              :type (simple-array (unsigned-byte 8) (32)))
  (body nil :type (or null beacon-block-body)))

;;; ============================================================================
;;; State Transition
;;; ============================================================================

(defun state-transition (state signed-block &key (validate t))
  "Execute state transition for a signed block.
   This is the main entry point for processing blocks.
   Returns the modified state or signals an error."
  (let ((block (if (listp signed-block)
                   (getf signed-block :message signed-block)
                   signed-block)))

    ;; Process slots up to block slot
    (process-slots state (beacon-block-slot block))

    ;; Verify state root if validating
    (when validate
      ;; Verify block slot is current
      (unless (= (beacon-block-slot block) (beacon-state-slot state))
        (error "Block slot ~D does not match state slot ~D"
               (beacon-block-slot block) (beacon-state-slot state))))

    ;; Process block
    (process-block state block)

    ;; Verify state root matches block's claimed state root
    (when validate
      (let ((computed-root (compute-state-root state)))
        (unless (equalp computed-root (beacon-block-state-root block))
          (error "State root mismatch"))))

    state))

;;; ============================================================================
;;; Slot Processing
;;; ============================================================================

(defun process-slots (state target-slot)
  "Process slots until target slot.
   Handles epoch boundaries and slot state updates."
  (when (> (beacon-state-slot state) target-slot)
    (error "Cannot process slots backwards: current ~D, target ~D"
           (beacon-state-slot state) target-slot))

  (loop while (< (beacon-state-slot state) target-slot)
        do (progn
             (process-slot state)
             ;; Process epoch at boundary
             (when (zerop (mod (1+ (beacon-state-slot state)) +slots-per-epoch+))
               (process-epoch state))
             ;; Advance slot
             (incf (beacon-state-slot state))))

  state)

(defun process-slot (state)
  "Process a single slot transition.
   Updates state roots and block roots."
  (let* ((slot (beacon-state-slot state))
         (previous-state-root (compute-state-root state))
         (previous-block-root (get-block-root-at-slot state slot)))

    ;; Cache state root
    (setf (aref (beacon-state-state-roots state)
                (mod slot +slots-per-historical-root+))
          previous-state-root)

    ;; Cache latest block header state root if empty
    ;; (Block roots are set during block processing)

    ;; Cache block root (copy from previous slot if no block)
    (when (and (> slot 0)
               (equalp previous-block-root
                       (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref (beacon-state-block-roots state)
                  (mod slot +slots-per-historical-root+))
            (get-block-root-at-slot state (1- slot))))))

;;; ============================================================================
;;; Block Processing
;;; ============================================================================

(defun process-block (state block)
  "Process a beacon block.
   Executes all block-level state transitions."
  ;; Verify proposer
  (let ((proposer-index (get-beacon-proposer-index state)))
    (unless (= proposer-index (beacon-block-proposer-index block))
      (error "Proposer index mismatch: expected ~D, got ~D"
             proposer-index (beacon-block-proposer-index block))))

  ;; Process RANDAO
  (process-randao state (beacon-block-body block))

  ;; Process ETH1 data
  (process-eth1-data state (beacon-block-body block))

  ;; Process operations
  (process-operations state (beacon-block-body block))

  ;; Store block root
  (let ((block-root (compute-block-root block)))
    (setf (aref (beacon-state-block-roots state)
                (mod (beacon-state-slot state) +slots-per-historical-root+))
          block-root))

  state)

;;; ============================================================================
;;; RANDAO Processing
;;; ============================================================================

(defun process-randao (state body)
  "Process RANDAO reveal and update mix."
  (let* ((epoch (get-current-epoch state))
         (proposer-index (get-beacon-proposer-index state))
         (proposer (get-validator state proposer-index))
         (randao-reveal (beacon-block-body-randao-reveal body)))

    ;; Verify RANDAO reveal (BLS signature of epoch)
    (let ((signing-root (compute-signing-root-epoch epoch)))
      (unless (bls-verify (validator-pubkey proposer) signing-root randao-reveal)
        (error "Invalid RANDAO reveal")))

    ;; Mix in RANDAO
    (let* ((mix-index (mod epoch +epochs-per-historical-vector+))
           (current-mix (aref (beacon-state-randao-mixes state) mix-index))
           (reveal-hash (sha256 randao-reveal))
           (new-mix (xor-bytes current-mix reveal-hash)))
      (setf (aref (beacon-state-randao-mixes state) mix-index) new-mix)))

  t)

(defun compute-signing-root-epoch (epoch)
  "Compute signing root for RANDAO reveal."
  (let ((epoch-bytes (integer-to-bytes epoch 8)))
    (sha256 epoch-bytes)))

;;; ============================================================================
;;; ETH1 Data Processing
;;; ============================================================================

(defun process-eth1-data (state body)
  "Process ETH1 data vote."
  (let ((eth1-data (beacon-block-body-eth1-data body)))
    (when eth1-data
      ;; Add vote
      (push eth1-data (beacon-state-eth1-data-votes state))

      ;; Check if we have majority
      (let ((vote-count (count-eth1-votes state eth1-data))
            (slots-per-period (* +epochs-per-eth1-voting-period+ +slots-per-epoch+)))
        (when (> (* vote-count 2) slots-per-period)
          (setf (beacon-state-eth1-data state) eth1-data)))))
  t)

(defun count-eth1-votes (state eth1-data)
  "Count votes for a specific ETH1 data."
  (count-if (lambda (vote)
              (and (equalp (eth1-data-deposit-root vote)
                           (eth1-data-deposit-root eth1-data))
                   (= (eth1-data-deposit-count vote)
                      (eth1-data-deposit-count eth1-data))
                   (equalp (eth1-data-block-hash vote)
                           (eth1-data-block-hash eth1-data))))
            (beacon-state-eth1-data-votes state)))

;;; ============================================================================
;;; Operations Processing
;;; ============================================================================

(defun process-operations (state body)
  "Process all operations in a block body."
  ;; Process proposer slashings
  (dolist (slashing (beacon-block-body-proposer-slashings body))
    (process-proposer-slashing state slashing))

  ;; Process attester slashings
  (dolist (slashing (beacon-block-body-attester-slashings body))
    (process-attester-slashing state slashing))

  ;; Process attestations
  (dolist (attestation (beacon-block-body-attestations body))
    (process-attestation state attestation))

  ;; Process deposits
  (dolist (deposit (beacon-block-body-deposits body))
    (process-deposit state deposit))

  ;; Process voluntary exits
  (dolist (exit (beacon-block-body-voluntary-exits body))
    (process-voluntary-exit state exit))

  t)

;;; ============================================================================
;;; Proposer Slashing
;;; ============================================================================

(defun process-proposer-slashing (state slashing)
  "Process a proposer slashing operation."
  (let* ((header-1 (getf slashing :signed-header-1))
         (header-2 (getf slashing :signed-header-2))
         (h1 (if (listp header-1) (getf header-1 :message header-1) header-1))
         (h2 (if (listp header-2) (getf header-2 :message header-2) header-2)))

    ;; Verify headers have same slot
    (unless (= (beacon-block-header-slot h1) (beacon-block-header-slot h2))
      (error "Proposer slashing headers have different slots"))

    ;; Verify headers are different
    (when (equalp h1 h2)
      (error "Proposer slashing headers are identical"))

    ;; Verify same proposer
    (unless (= (beacon-block-header-proposer-index h1)
               (beacon-block-header-proposer-index h2))
      (error "Proposer slashing headers have different proposers"))

    (let* ((proposer-index (beacon-block-header-proposer-index h1))
           (proposer (get-validator state proposer-index))
           (epoch (compute-epoch-at-slot (beacon-block-header-slot h1))))

      ;; Verify proposer is slashable
      (unless (validator-is-slashable-p proposer epoch)
        (error "Proposer is not slashable"))

      ;; Slash the proposer
      (slash-validator state proposer-index)))

  t)

;;; ============================================================================
;;; Attester Slashing
;;; ============================================================================

(defun process-attester-slashing (state slashing)
  "Process an attester slashing operation."
  (let* ((att-1 (getf slashing :attestation-1))
         (att-2 (getf slashing :attestation-2))
         (indices-1 (getf att-1 :attesting-indices))
         (indices-2 (getf att-2 :attesting-indices))
         (data-1 (getf att-1 :data))
         (data-2 (getf att-2 :data)))

    ;; Verify slashing conditions
    (unless (is-slashable-attestation-data-p data-1 data-2)
      (error "Attestations are not slashable"))

    ;; Slash all validators in both attestations
    (let ((slashed-indices (intersection indices-1 indices-2))
          (current-epoch (get-current-epoch state)))
      (dolist (idx slashed-indices)
        (let ((validator (get-validator state idx)))
          (when (validator-is-slashable-p validator current-epoch)
            (slash-validator state idx))))))

  t)

(defun is-slashable-attestation-data-p (data-1 data-2)
  "Check if two attestation data objects represent a slashable offense."
  (or
   ;; Double vote: same target epoch, different data
   (and (= (checkpoint-epoch (attestation-data-target data-1))
           (checkpoint-epoch (attestation-data-target data-2)))
        (not (attestation-data-equal-p data-1 data-2)))
   ;; Surround vote: source1 < source2 < target2 < target1
   (and (< (checkpoint-epoch (attestation-data-source data-1))
           (checkpoint-epoch (attestation-data-source data-2)))
        (< (checkpoint-epoch (attestation-data-target data-2))
           (checkpoint-epoch (attestation-data-target data-1))))))

;;; ============================================================================
;;; Deposit Processing
;;; ============================================================================

(defun process-deposit (state deposit)
  "Process a validator deposit."
  (let* ((pubkey (getf deposit :pubkey))
         (withdrawal-credentials (getf deposit :withdrawal-credentials))
         (amount (getf deposit :amount))
         (signature (getf deposit :signature)))

    ;; Increment deposit index
    (incf (beacon-state-eth1-deposit-index state))

    ;; Check if validator already exists
    (multiple-value-bind (existing-validator existing-index)
        (get-validator-by-pubkey state pubkey)

      (if existing-validator
          ;; Existing validator - add to balance
          (increase-balance state existing-index amount)

          ;; New validator - verify signature and add
          (when (bls-verify pubkey
                            (compute-deposit-signing-root pubkey withdrawal-credentials amount)
                            signature)
            (let ((validator (make-validator
                              :pubkey pubkey
                              :withdrawal-credentials withdrawal-credentials
                              :effective-balance (min (floor-to-increment amount +effective-balance-increment+)
                                                      +max-effective-balance+)
                              :activation-eligibility-epoch +far-future-epoch+
                              :activation-epoch +far-future-epoch+
                              :exit-epoch +far-future-epoch+
                              :withdrawable-epoch +far-future-epoch+)))
              (add-validator state validator amount))))))

  t)

(defun compute-deposit-signing-root (pubkey withdrawal-credentials amount)
  "Compute signing root for deposit signature verification."
  (sha256 (concat-bytes pubkey withdrawal-credentials (integer-to-bytes amount 8))))

;;; ============================================================================
;;; Voluntary Exit Processing
;;; ============================================================================

(defun process-voluntary-exit (state signed-exit)
  "Process a voluntary exit."
  (let* ((exit (if (listp signed-exit)
                   (getf signed-exit :message signed-exit)
                   signed-exit))
         (validator-index (getf exit :validator-index))
         (exit-epoch (getf exit :epoch))
         (validator (get-validator state validator-index))
         (current-epoch (get-current-epoch state)))

    ;; Verify validator is active
    (unless (validator-is-active-p validator current-epoch)
      (error "Validator ~D is not active" validator-index))

    ;; Verify exit epoch has passed
    (unless (>= current-epoch exit-epoch)
      (error "Exit epoch ~D has not arrived" exit-epoch))

    ;; Verify validator has been active long enough
    (unless (>= current-epoch
                (+ (validator-activation-epoch validator) +shard-committee-period+))
      (error "Validator ~D has not been active long enough" validator-index))

    ;; Verify not already exiting
    (unless (= (validator-exit-epoch validator) +far-future-epoch+)
      (error "Validator ~D is already exiting" validator-index))

    ;; Initiate exit
    (initiate-validator-exit state validator-index))

  t)

;;; ============================================================================
;;; State/Block Root Computation
;;; ============================================================================

(defun compute-state-root (state)
  "Compute the root hash of the beacon state.
   Uses simplified SSZ-like hashing."
  (let ((data (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Hash key state components
    (let ((slot-bytes (integer-to-bytes (beacon-state-slot state) 8))
          (validator-count-bytes (integer-to-bytes (length (beacon-state-validators state)) 8)))
      (replace data slot-bytes :start1 0)
      (replace data validator-count-bytes :start1 8)
      (replace data (beacon-state-genesis-validators-root state) :start1 16)

      ;; Include checkpoint epochs
      (let ((fin-epoch (integer-to-bytes (get-finalized-epoch state) 8))
            (just-epoch (integer-to-bytes (get-justified-epoch state) 8)))
        (replace data fin-epoch :start1 48)
        (replace data just-epoch :start1 56))

      ;; Hash balances
      (when (> (length (beacon-state-balances state)) 0)
        (let ((balance-sum (reduce #'+ (beacon-state-balances state))))
          (replace data (integer-to-bytes balance-sum 8) :start1 64))))

    (sha256 data)))

(defun compute-block-root (block)
  "Compute the root hash of a beacon block."
  (let ((data (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace data (integer-to-bytes (beacon-block-slot block) 8) :start1 0)
    (replace data (integer-to-bytes (beacon-block-proposer-index block) 8) :start1 8)
    (replace data (beacon-block-parent-root block) :start1 16)
    (sha256 data)))

;;; ============================================================================
;;; Genesis State Creation
;;; ============================================================================

(defun create-genesis-state (validators genesis-time)
  "Create a genesis beacon state with initial validators.
   VALIDATORS: list of (pubkey withdrawal-credentials balance) tuples
   GENESIS-TIME: Unix timestamp for genesis"
  (let ((state (make-beacon-state :genesis-time genesis-time)))

    ;; Add validators
    (dolist (v validators)
      (destructuring-bind (pubkey withdrawal-creds balance) v
        (let ((validator (make-validator
                          :pubkey pubkey
                          :withdrawal-credentials withdrawal-creds
                          :effective-balance (min (floor-to-increment balance +effective-balance-increment+)
                                                  +max-effective-balance+)
                          :activation-eligibility-epoch 0
                          :activation-epoch 0
                          :exit-epoch +far-future-epoch+
                          :withdrawable-epoch +far-future-epoch+)))
          (add-validator state validator balance))))

    ;; Compute genesis validators root
    (let ((validators-hash (compute-validators-root state)))
      (setf (beacon-state-genesis-validators-root state) validators-hash))

    ;; Initialize RANDAO mixes
    (let ((genesis-mix (sha256 (integer-to-bytes genesis-time 8))))
      (loop for i from 0 below +epochs-per-historical-vector+
            do (setf (aref (beacon-state-randao-mixes state) i)
                     (copy-seq genesis-mix))))

    state))

(defun compute-validators-root (state)
  "Compute Merkle root of validator pubkeys."
  (let ((data (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0))
        (validators (beacon-state-validators state)))
    (when (> (length validators) 0)
      ;; Hash first few pubkeys
      (loop for i from 0 below (min 4 (length validators))
            for v = (aref validators i)
            do (replace data (validator-pubkey v) :start1 (* i 48) :end1 (+ (* i 48) 48))))
    (sha256 data)))
