;;;; state.lisp
;;;;
;;;; Core beacon state structure and constants
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:cl-beacon-state-machine)

;;; ============================================================================
;;; Protocol Constants
;;; ============================================================================

;;; Time parameters
(defconstant +seconds-per-slot+ 12
  "Seconds per slot (12 seconds).")

(defconstant +slots-per-epoch+ 32
  "Slots per epoch (32 slots = 6.4 minutes).")

(defconstant +min-seed-lookahead+ 1
  "Minimum epochs before seed is used for shuffling.")

(defconstant +max-seed-lookahead+ 4
  "Maximum epochs before seed is used.")

(defconstant +epochs-per-eth1-voting-period+ 64
  "Epochs per Eth1 voting period.")

(defconstant +slots-per-historical-root+ 8192
  "Slots stored in block/state roots arrays.")

(defconstant +epochs-per-historical-vector+ 65536
  "RANDAO mix history length.")

(defconstant +epochs-per-slashings-vector+ 8192
  "Slashings accumulator history length.")

(defconstant +min-epochs-to-inactivity-penalty+ 4
  "Epochs before inactivity leak begins.")

(defconstant +shard-committee-period+ 256
  "Epochs a validator must be active before exit.")

;;; Validator limits
(defconstant +max-committees-per-slot+ 64
  "Maximum committees per slot.")

(defconstant +target-committee-size+ 128
  "Target validators per committee.")

(defconstant +max-validators-per-committee+ 2048
  "Maximum validators in a single committee.")

(defconstant +shuffle-round-count+ 90
  "Number of rounds in swap-or-not shuffle.")

;;; Balance parameters
(defconstant +min-deposit-amount+ 1000000000
  "Minimum deposit (1 ETH in Gwei).")

(defconstant +max-effective-balance+ 32000000000
  "Maximum effective balance (32 ETH in Gwei).")

(defconstant +effective-balance-increment+ 1000000000
  "Effective balance increment (1 ETH in Gwei).")

(defconstant +min-attestation-inclusion-delay+ 1
  "Minimum slots between attestation and inclusion.")

(defconstant +far-future-epoch+ (1- (ash 1 64))
  "Sentinel for unset epoch values.")

;;; Reward/penalty parameters
(defconstant +proposer-reward-quotient+ 8
  "Proposer reward quotient.")

(defconstant +inactivity-penalty-quotient+ 67108864
  "Inactivity penalty quotient (2^26).")

(defconstant +min-slashing-penalty-quotient+ 128
  "Minimum slashing penalty quotient.")

(defconstant +whistleblower-reward-quotient+ 512
  "Whistleblower reward quotient.")

(defconstant +base-reward-factor+ 64
  "Base reward factor.")

(defconstant +weight-denominator+ 64
  "Weight denominator for reward calculations.")

;;; Participation weights
(defconstant +timely-source-weight+ 14
  "Weight for timely source attestation.")

(defconstant +timely-target-weight+ 26
  "Weight for timely target attestation.")

(defconstant +timely-head-weight+ 14
  "Weight for timely head attestation.")

(defconstant +proposer-weight+ 8
  "Proposer reward weight.")

;;; Participation flag indices
(defconstant +timely-source-flag-index+ 0)
(defconstant +timely-target-flag-index+ 1)
(defconstant +timely-head-flag-index+ 2)

;;; ============================================================================
;;; Core Types
;;; ============================================================================

(defstruct (checkpoint (:constructor make-checkpoint (&key (epoch 0) root)))
  "Checkpoint for justification/finalization."
  (epoch 0 :type (integer 0))
  (root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
        :type (simple-array (unsigned-byte 8) (32))))

(defun checkpoint-equal-p (a b)
  "Check if two checkpoints are equal."
  (and (= (checkpoint-epoch a) (checkpoint-epoch b))
       (equalp (checkpoint-root a) (checkpoint-root b))))

(defstruct (fork (:constructor make-fork (&key previous-version current-version (epoch 0))))
  "Fork information for signature domains."
  (previous-version (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)
                    :type (simple-array (unsigned-byte 8) (4)))
  (current-version (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)
                   :type (simple-array (unsigned-byte 8) (4)))
  (epoch 0 :type (integer 0)))

(defstruct (eth1-data (:constructor make-eth1-data (&key deposit-root (deposit-count 0) block-hash)))
  "Eth1 chain data for deposits."
  (deposit-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
                :type (simple-array (unsigned-byte 8) (32)))
  (deposit-count 0 :type (integer 0))
  (block-hash (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
              :type (simple-array (unsigned-byte 8) (32))))

;;; ============================================================================
;;; BeaconState Structure
;;; ============================================================================

(defstruct (beacon-state
            (:constructor %make-beacon-state)
            (:copier nil)
            (:predicate beacon-state-p))
  "Complete beacon chain state at a specific slot."
  ;; Genesis
  (genesis-time 0 :type (integer 0))
  (genesis-validators-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
                           :type (simple-array (unsigned-byte 8) (32)))
  ;; Versioning
  (slot 0 :type (integer 0))
  (fork nil :type (or null fork))
  ;; History
  (block-roots (make-array +slots-per-historical-root+
                           :initial-element (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
               :type simple-vector)
  (state-roots (make-array +slots-per-historical-root+
                           :initial-element (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
               :type simple-vector)
  ;; Eth1
  (eth1-data nil :type (or null eth1-data))
  (eth1-data-votes nil :type list)
  (eth1-deposit-index 0 :type (integer 0))
  ;; Registry
  (validators (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (balances (make-array 0 :element-type '(integer 0) :adjustable t :fill-pointer 0) :type vector)
  ;; Randomness
  (randao-mixes (make-array +epochs-per-historical-vector+
                            :initial-element (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
                :type simple-vector)
  ;; Slashings
  (slashings (make-array +epochs-per-slashings-vector+ :element-type '(integer 0) :initial-element 0)
             :type (simple-array (integer 0) (*)))
  ;; Participation (Altair+)
  (previous-epoch-participation (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
                                :type vector)
  (current-epoch-participation (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
                               :type vector)
  ;; Finality
  (justification-bits (make-array 4 :element-type 'bit :initial-element 0)
                      :type (simple-bit-vector 4))
  (previous-justified-checkpoint nil :type (or null checkpoint))
  (current-justified-checkpoint nil :type (or null checkpoint))
  (finalized-checkpoint nil :type (or null checkpoint))
  ;; Inactivity
  (inactivity-scores (make-array 0 :element-type '(integer 0) :adjustable t :fill-pointer 0)
                     :type vector)
  ;; Threading
  (lock (sb-thread:make-mutex :name "beacon-state-lock")))

(defun make-beacon-state (&key (genesis-time 0) genesis-validators-root)
  "Create a new beacon state."
  (let ((state (%make-beacon-state
                :genesis-time genesis-time
                :genesis-validators-root (or genesis-validators-root
                                             (make-array 32 :element-type '(unsigned-byte 8)
                                                         :initial-element 0))
                :fork (make-fork)
                :eth1-data (make-eth1-data)
                :previous-justified-checkpoint (make-checkpoint)
                :current-justified-checkpoint (make-checkpoint)
                :finalized-checkpoint (make-checkpoint))))
    state))

;;; ============================================================================
;;; State Accessors
;;; ============================================================================

(defun get-current-epoch (state)
  "Get the current epoch from the state's slot."
  (compute-epoch-at-slot (beacon-state-slot state)))

(defun get-previous-epoch (state)
  "Get the previous epoch (or 0 if at genesis)."
  (let ((current (get-current-epoch state)))
    (if (zerop current) 0 (1- current))))

(defun get-next-epoch (state)
  "Get the next epoch."
  (1+ (get-current-epoch state)))

(defun compute-epoch-at-slot (slot)
  "Compute epoch number for a slot."
  (floor slot +slots-per-epoch+))

(defun compute-start-slot-at-epoch (epoch)
  "Compute first slot of an epoch."
  (* epoch +slots-per-epoch+))

(defun is-epoch-boundary-p (state)
  "Check if state is at an epoch boundary."
  (zerop (mod (beacon-state-slot state) +slots-per-epoch+)))

(defun get-randao-mix (state epoch)
  "Get RANDAO mix for an epoch."
  (aref (beacon-state-randao-mixes state)
        (mod epoch +epochs-per-historical-vector+)))

(defun get-block-root (state epoch)
  "Get block root at start of epoch."
  (get-block-root-at-slot state (compute-start-slot-at-epoch epoch)))

(defun get-block-root-at-slot (state slot)
  "Get block root at a specific slot."
  (aref (beacon-state-block-roots state)
        (mod slot +slots-per-historical-root+)))

(defun get-finalized-epoch (state)
  "Get the finalized epoch."
  (checkpoint-epoch (beacon-state-finalized-checkpoint state)))

(defun get-justified-epoch (state)
  "Get the current justified epoch."
  (checkpoint-epoch (beacon-state-current-justified-checkpoint state)))

(defun get-finality-delay (state)
  "Get epochs since last finality."
  (- (get-previous-epoch state) (get-finalized-epoch state)))
