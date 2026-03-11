;;;; epoch.lisp
;;;;
;;;; Epoch processing for cl-beacon-state-machine
;;;; Implements justification, finalization, rewards, penalties, and registry updates
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:cl-beacon-state-machine)

;;; ============================================================================
;;; Epoch Processing Entry Point
;;; ============================================================================

(defun process-epoch (state)
  "Process all epoch transitions. Called at epoch boundaries."
  (process-justification-and-finalization state)
  (process-inactivity-updates state)
  (process-rewards-and-penalties state)
  (process-registry-updates state)
  (process-slashings state)
  (process-eth1-data-reset state)
  (process-effective-balance-updates state)
  (process-slashings-reset state)
  (process-randao-mixes-reset state)
  (process-participation-flag-updates state)
  t)

;;; ============================================================================
;;; Justification and Finalization
;;; ============================================================================

(defun process-justification-and-finalization (state)
  "Update justification and finalization checkpoints.
   Implements the Casper FFG finality gadget."
  (let* ((current-epoch (get-current-epoch state))
         (previous-epoch (get-previous-epoch state)))

    ;; Skip if not enough history
    (when (<= current-epoch 1)
      (return-from process-justification-and-finalization nil))

    ;; Get previous and current epoch attestations
    (let* ((previous-target-balance (get-epoch-target-attesting-balance state previous-epoch))
           (current-target-balance (get-epoch-target-attesting-balance state current-epoch))
           (total-active-balance (get-total-active-balance state))
           (justification-bits (copy-seq (beacon-state-justification-bits state))))

      ;; Shift justification bits
      (setf (bit justification-bits 3) (bit justification-bits 2))
      (setf (bit justification-bits 2) (bit justification-bits 1))
      (setf (bit justification-bits 1) (bit justification-bits 0))
      (setf (bit justification-bits 0) 0)

      ;; Process previous epoch justification (2/3 supermajority)
      (when (>= (* previous-target-balance 3) (* total-active-balance 2))
        (setf (beacon-state-previous-justified-checkpoint state)
              (make-checkpoint :epoch previous-epoch
                               :root (get-block-root state previous-epoch)))
        (setf (bit justification-bits 1) 1))

      ;; Process current epoch justification
      (when (>= (* current-target-balance 3) (* total-active-balance 2))
        (setf (beacon-state-current-justified-checkpoint state)
              (make-checkpoint :epoch current-epoch
                               :root (get-block-root state current-epoch)))
        (setf (bit justification-bits 0) 1))

      ;; Update state justification bits
      (setf (beacon-state-justification-bits state) justification-bits)

      ;; Process finalization
      (process-finalization state justification-bits current-epoch))))

(defun process-finalization (state justification-bits current-epoch)
  "Update finalized checkpoint based on justification history."
  (let ((bits justification-bits)
        (old-previous-justified (beacon-state-previous-justified-checkpoint state))
        (old-current-justified (beacon-state-current-justified-checkpoint state)))

    ;; The 2nd/3rd/4th most recent epochs are justified, the 2nd using the 4th as source
    (when (and (= (bit bits 1) 1)
               (= (bit bits 2) 1)
               (= (bit bits 3) 1)
               (= (checkpoint-epoch old-previous-justified) (- current-epoch 3)))
      (setf (beacon-state-finalized-checkpoint state) old-previous-justified))

    ;; The 2nd/3rd most recent epochs are justified, the 2nd using the 3rd as source
    (when (and (= (bit bits 1) 1)
               (= (bit bits 2) 1)
               (= (checkpoint-epoch old-previous-justified) (- current-epoch 2)))
      (setf (beacon-state-finalized-checkpoint state) old-previous-justified))

    ;; The 1st/2nd/3rd most recent epochs are justified, the 1st using the 3rd as source
    (when (and (= (bit bits 0) 1)
               (= (bit bits 1) 1)
               (= (bit bits 2) 1)
               (= (checkpoint-epoch old-current-justified) (- current-epoch 2)))
      (setf (beacon-state-finalized-checkpoint state) old-current-justified))

    ;; The 1st/2nd most recent epochs are justified, the 1st using the 2nd as source
    (when (and (= (bit bits 0) 1)
               (= (bit bits 1) 1)
               (= (checkpoint-epoch old-current-justified) (- current-epoch 1)))
      (setf (beacon-state-finalized-checkpoint state) old-current-justified))))

(defun get-epoch-target-attesting-balance (state epoch)
  "Get total balance of validators that attested to the target in an epoch."
  (let ((indices (get-unslashed-participating-indices state +timely-target-flag-index+ epoch)))
    (get-total-balance state indices)))

;;; ============================================================================
;;; Inactivity Updates
;;; ============================================================================

(defun process-inactivity-updates (state)
  "Update inactivity scores for validators."
  (let* ((current-epoch (get-current-epoch state))
         (previous-epoch (get-previous-epoch state))
         (finality-delay (get-finality-delay state))
         (is-in-inactivity-leak (> finality-delay +min-epochs-to-inactivity-penalty+))
         (participating-indices (get-unslashed-participating-indices
                                 state +timely-target-flag-index+ previous-epoch)))

    (loop for i from 0 below (length (beacon-state-validators state))
          for validator = (aref (beacon-state-validators state) i)
          when (validator-is-active-p validator current-epoch)
            do (let ((is-participating (member i participating-indices)))
                 (cond
                   ;; Decrease score for participating validators
                   (is-participating
                    (when (> (aref (beacon-state-inactivity-scores state) i) 0)
                      (decf (aref (beacon-state-inactivity-scores state) i) 1)))
                   ;; Increase score during inactivity leak
                   (is-in-inactivity-leak
                    (incf (aref (beacon-state-inactivity-scores state) i) 4)))))))

;;; ============================================================================
;;; Rewards and Penalties
;;; ============================================================================

(defun process-rewards-and-penalties (state)
  "Apply epoch rewards and penalties to all validators."
  (let ((current-epoch (get-current-epoch state)))
    ;; Skip genesis epoch
    (when (= current-epoch 0)
      (return-from process-rewards-and-penalties nil))

    (multiple-value-bind (rewards penalties)
        (compute-epoch-rewards-penalties state)
      ;; Apply rewards and penalties
      (loop for i from 0 below (length rewards)
            do (progn
                 (increase-balance state i (aref rewards i))
                 (decrease-balance state i (aref penalties i)))))
    t))

(defun compute-epoch-rewards-penalties (state)
  "Compute rewards and penalties for all validators.
   Returns (values rewards-array penalties-array)."
  (let* ((validator-count (length (beacon-state-validators state)))
         (rewards (make-array validator-count :element-type '(integer 0) :initial-element 0))
         (penalties (make-array validator-count :element-type '(integer 0) :initial-element 0))
         (previous-epoch (get-previous-epoch state))
         (total-active-balance (get-total-active-balance state previous-epoch))
         (sqrt-balance (integer-square-root total-active-balance))
         (is-in-inactivity-leak (> (get-finality-delay state) +min-epochs-to-inactivity-penalty+)))

    ;; Get participating validator sets for each flag
    (let ((source-participating (get-unslashed-participating-indices
                                 state +timely-source-flag-index+ previous-epoch))
          (target-participating (get-unslashed-participating-indices
                                 state +timely-target-flag-index+ previous-epoch))
          (head-participating (get-unslashed-participating-indices
                               state +timely-head-flag-index+ previous-epoch)))

      ;; Get participating balances
      (let ((source-balance (get-total-balance state source-participating))
            (target-balance (get-total-balance state target-participating))
            (head-balance (get-total-balance state head-participating)))

        ;; Process each eligible validator
        (loop for i from 0 below validator-count
              for validator = (aref (beacon-state-validators state) i)
              when (validator-is-active-p validator previous-epoch)
                do (let* ((effective-balance (validator-effective-balance validator))
                          (base-reward (if (zerop sqrt-balance)
                                           0
                                           (floor (* effective-balance +base-reward-factor+)
                                                  (* sqrt-balance +weight-denominator+))))
                          (is-slashed (validator-slashed validator)))

                     ;; Process source reward/penalty
                     (process-flag-reward-penalty
                      state i rewards penalties
                      base-reward +timely-source-weight+
                      (and (not is-slashed) (member i source-participating))
                      source-balance total-active-balance is-in-inactivity-leak)

                     ;; Process target reward/penalty
                     (process-flag-reward-penalty
                      state i rewards penalties
                      base-reward +timely-target-weight+
                      (and (not is-slashed) (member i target-participating))
                      target-balance total-active-balance is-in-inactivity-leak)

                     ;; Process head reward/penalty
                     (process-flag-reward-penalty
                      state i rewards penalties
                      base-reward +timely-head-weight+
                      (and (not is-slashed) (member i head-participating))
                      head-balance total-active-balance is-in-inactivity-leak)

                     ;; Apply inactivity penalty
                     (when (and is-in-inactivity-leak
                                (not (member i target-participating)))
                       (let ((penalty (floor (* effective-balance
                                                (aref (beacon-state-inactivity-scores state) i))
                                             (* +inactivity-penalty-quotient+ +weight-denominator+))))
                         (incf (aref penalties i) penalty)))))))

    (values rewards penalties)))

(defun process-flag-reward-penalty (state index rewards penalties
                                    base-reward weight is-participating
                                    participating-balance total-balance is-leak)
  "Process reward or penalty for a single participation flag."
  (declare (ignore state))
  (let ((reward-component (floor (* base-reward weight) +weight-denominator+)))
    (if is-participating
        ;; Reward for participation
        (unless is-leak
          (let ((reward (floor (* reward-component participating-balance)
                               total-balance)))
            (incf (aref rewards index) reward)))
        ;; Penalty for non-participation
        (incf (aref penalties index) reward-component))))

;;; ============================================================================
;;; Registry Updates
;;; ============================================================================

(defun process-registry-updates (state)
  "Process validator registry updates.
   Activates eligible validators and processes exits."
  (let* ((current-epoch (get-current-epoch state))
         (activation-exit-epoch (compute-activation-exit-epoch current-epoch))
         (churn-limit (get-validator-churn-limit state))
         (activation-queue nil))

    ;; Process validator eligibility and exits
    (loop for i from 0 below (length (beacon-state-validators state))
          for validator = (aref (beacon-state-validators state) i)
          do (progn
               ;; Check eligibility for activation queue
               (when (and (= (validator-activation-eligibility-epoch validator) +far-future-epoch+)
                          (>= (validator-effective-balance validator) +max-effective-balance+))
                 (setf (validator-activation-eligibility-epoch validator) (1+ current-epoch)))

               ;; Add to activation queue if eligible
               (when (and (validator-is-eligible-for-activation-p validator current-epoch)
                          (= (validator-activation-epoch validator) +far-future-epoch+))
                 (push (cons i validator) activation-queue))))

    ;; Sort activation queue by eligibility epoch, then by index
    (setf activation-queue
          (sort activation-queue
                (lambda (a b)
                  (let ((ea (validator-activation-eligibility-epoch (cdr a)))
                        (eb (validator-activation-eligibility-epoch (cdr b))))
                    (if (= ea eb)
                        (< (car a) (car b))
                        (< ea eb))))))

    ;; Activate up to churn limit
    (loop for (idx . validator) in activation-queue
          for count from 0 below churn-limit
          do (setf (validator-activation-epoch validator) activation-exit-epoch))

    t))

(defun validator-is-eligible-for-activation-p (validator current-epoch)
  "Check if validator is eligible for activation."
  (and (<= (validator-activation-eligibility-epoch validator) current-epoch)
       (= (validator-activation-epoch validator) +far-future-epoch+)))

;;; ============================================================================
;;; Slashings Processing
;;; ============================================================================

(defun process-slashings (state)
  "Apply slashing penalties proportional to total slashings."
  (let* ((current-epoch (get-current-epoch state))
         (total-slashings (reduce #'+ (beacon-state-slashings state)))
         (total-balance (get-total-active-balance state))
         (adjusted-total-slashings (min (* total-slashings 3) total-balance)))

    ;; Process each slashed validator in withdrawable epoch
    (loop for i from 0 below (length (beacon-state-validators state))
          for validator = (aref (beacon-state-validators state) i)
          when (and (validator-slashed validator)
                    (= (+ current-epoch (floor +epochs-per-slashings-vector+ 2))
                       (validator-withdrawable-epoch validator)))
            do (let* ((effective-balance (validator-effective-balance validator))
                      (penalty (floor (* (floor (* effective-balance adjusted-total-slashings)
                                                total-balance)
                                         1)
                                      +min-slashing-penalty-quotient+)))
                 (decrease-balance state i penalty))))
  t)

(defun process-slashings-reset (state)
  "Reset slashings accumulator slot for next epoch."
  (let ((next-epoch (1+ (get-current-epoch state))))
    (setf (aref (beacon-state-slashings state)
                (mod next-epoch +epochs-per-slashings-vector+))
          0)))

;;; ============================================================================
;;; Effective Balance Updates
;;; ============================================================================

(defun process-effective-balance-updates (state)
  "Update effective balances with hysteresis."
  (let ((hysteresis-increment (floor +effective-balance-increment+ 4))
        (downward-threshold (* (floor +effective-balance-increment+ 4) 3))
        (upward-threshold (* (floor +effective-balance-increment+ 4) 5)))

    (loop for i from 0 below (length (beacon-state-validators state))
          for validator = (aref (beacon-state-validators state) i)
          for balance = (aref (beacon-state-balances state) i)
          for effective = (validator-effective-balance validator)
          do (let ((low-balance (+ effective downward-threshold))
                   (high-balance (+ effective upward-threshold)))
               ;; Apply hysteresis for balance changes
               (when (or (< balance low-balance)
                         (>= balance high-balance))
                 (setf (validator-effective-balance validator)
                       (min (floor-to-increment balance +effective-balance-increment+)
                            +max-effective-balance+))))))
  t)

(defun floor-to-increment (n increment)
  "Floor n to the nearest increment."
  (* (floor n increment) increment))

;;; ============================================================================
;;; ETH1 Data Processing
;;; ============================================================================

(defun process-eth1-data-reset (state)
  "Reset ETH1 data votes at end of voting period."
  (let* ((current-epoch (get-current-epoch state))
         (slots-per-period (* +epochs-per-eth1-voting-period+ +slots-per-epoch+)))
    (when (zerop (mod (1+ (beacon-state-slot state)) slots-per-period))
      (setf (beacon-state-eth1-data-votes state) nil)))
  t)

;;; ============================================================================
;;; RANDAO Mix Updates
;;; ============================================================================

(defun process-randao-mixes-reset (state)
  "Copy current RANDAO mix to next epoch slot."
  (let* ((current-epoch (get-current-epoch state))
         (next-epoch (1+ current-epoch))
         (current-mix (get-randao-mix state current-epoch)))
    (setf (aref (beacon-state-randao-mixes state)
                (mod next-epoch +epochs-per-historical-vector+))
          (copy-seq current-mix)))
  t)

;;; ============================================================================
;;; Participation Flag Updates
;;; ============================================================================

(defun process-participation-flag-updates (state)
  "Rotate participation flags at epoch boundary."
  (rotate-participation state)
  t)
