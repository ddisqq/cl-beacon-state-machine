;;;; attestation.lisp
;;;;
;;;; Attestation structures and processing for cl-beacon-state-machine
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:cl-beacon-state-machine)

;;; ============================================================================
;;; Attestation Data Structure
;;; ============================================================================

(defstruct (attestation-data
            (:constructor make-attestation-data
                (&key (slot 0) (index 0) beacon-block-root source target))
            (:copier nil)
            (:predicate attestation-data-p))
  "Core attestation data signed by validators."
  ;; LMD GHOST vote
  (slot 0 :type (integer 0))
  (index 0 :type (integer 0))  ; Committee index
  (beacon-block-root (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
                     :type (simple-array (unsigned-byte 8) (32)))
  ;; FFG vote
  (source nil :type (or null checkpoint))
  (target nil :type (or null checkpoint)))

(defun attestation-data-equal-p (a b)
  "Check if two attestation data objects are equal."
  (and (= (attestation-data-slot a) (attestation-data-slot b))
       (= (attestation-data-index a) (attestation-data-index b))
       (equalp (attestation-data-beacon-block-root a)
               (attestation-data-beacon-block-root b))
       (or (and (null (attestation-data-source a))
                (null (attestation-data-source b)))
           (and (attestation-data-source a)
                (attestation-data-source b)
                (checkpoint-equal-p (attestation-data-source a)
                                    (attestation-data-source b))))
       (or (and (null (attestation-data-target a))
                (null (attestation-data-target b)))
           (and (attestation-data-target a)
                (attestation-data-target b)
                (checkpoint-equal-p (attestation-data-target a)
                                    (attestation-data-target b))))))

;;; ============================================================================
;;; Attestation Structure
;;; ============================================================================

(defstruct (attestation
            (:constructor make-attestation
                (&key aggregation-bits data signature))
            (:copier nil)
            (:predicate attestation-p))
  "Signed attestation from validators."
  ;; Bitfield of participating validators
  (aggregation-bits (make-array 0 :element-type 'bit :initial-element 0)
                    :type simple-bit-vector)
  ;; Attestation data
  (data nil :type (or null attestation-data))
  ;; Aggregate BLS signature
  (signature (make-array 96 :element-type '(unsigned-byte 8) :initial-element 0)
             :type (simple-array (unsigned-byte 8) (96))))

;;; ============================================================================
;;; Attestation Validation
;;; ============================================================================

(defun validate-attestation (state attestation)
  "Validate an attestation against the current state.
   Returns (values valid-p error-message)."
  (let* ((data (attestation-data attestation))
         (att-slot (attestation-data-slot data))
         (att-index (attestation-data-index data))
         (att-epoch (compute-epoch-at-slot att-slot))
         (current-epoch (get-current-epoch state))
         (previous-epoch (get-previous-epoch state)))

    ;; Check epoch bounds
    (unless (or (= att-epoch current-epoch)
                (= att-epoch previous-epoch))
      (return-from validate-attestation
        (values nil "Attestation epoch not current or previous")))

    ;; Check source checkpoint
    (let ((justified (if (= att-epoch current-epoch)
                         (beacon-state-current-justified-checkpoint state)
                         (beacon-state-previous-justified-checkpoint state))))
      (unless (and (attestation-data-source data)
                   (checkpoint-equal-p (attestation-data-source data) justified))
        (return-from validate-attestation
          (values nil "Invalid source checkpoint"))))

    ;; Check target epoch matches attestation epoch
    (let ((target (attestation-data-target data)))
      (unless (and target (= (checkpoint-epoch target) att-epoch))
        (return-from validate-attestation
          (values nil "Target epoch mismatch"))))

    ;; Check slot bounds (not from future, not too old)
    (let ((min-slot (+ att-slot +min-attestation-inclusion-delay+)))
      (unless (>= (beacon-state-slot state) min-slot)
        (return-from validate-attestation
          (values nil "Attestation included too early"))))

    ;; Check committee index bounds
    (let ((committees-per-slot (get-committee-count-per-slot state att-epoch)))
      (unless (< att-index committees-per-slot)
        (return-from validate-attestation
          (values nil "Invalid committee index"))))

    ;; Check aggregation bits length matches committee
    (let* ((committee (get-beacon-committee state att-slot att-index))
           (expected-len (length committee))
           (actual-len (length (attestation-aggregation-bits attestation))))
      (unless (= actual-len expected-len)
        (return-from validate-attestation
          (values nil (format nil "Aggregation bits length mismatch: expected ~D, got ~D"
                              expected-len actual-len)))))

    ;; All checks passed
    (values t nil)))

;;; ============================================================================
;;; Attesting Indices
;;; ============================================================================

(defun get-attesting-indices (state attestation)
  "Get the indices of validators attesting in this attestation."
  (let* ((data (attestation-data attestation))
         (committee (get-beacon-committee state
                                          (attestation-data-slot data)
                                          (attestation-data-index data)))
         (bits (attestation-aggregation-bits attestation)))
    (loop for i from 0 below (length bits)
          when (= (bit bits i) 1)
            collect (nth i committee))))

(defun get-indexed-attestation (state attestation)
  "Convert attestation to indexed form with sorted attesting indices."
  (let ((indices (sort (copy-list (get-attesting-indices state attestation)) #'<)))
    (list :attesting-indices indices
          :data (attestation-data attestation)
          :signature (attestation-signature attestation))))

;;; ============================================================================
;;; Participation Flag Computation
;;; ============================================================================

(defun get-attestation-participation-flags (state attestation inclusion-delay)
  "Compute participation flags for an attestation.
   Returns a list of flag indices (0=source, 1=target, 2=head) that should be set."
  (let* ((data (attestation-data attestation))
         (att-slot (attestation-data-slot data))
         (att-epoch (compute-epoch-at-slot att-slot))
         (flags nil)
         (justified (if (= att-epoch (get-current-epoch state))
                        (beacon-state-current-justified-checkpoint state)
                        (beacon-state-previous-justified-checkpoint state)))
         (target-root (get-block-root state att-epoch))
         (head-root (get-block-root-at-slot state att-slot)))

    ;; Check source (justified checkpoint)
    (when (and (attestation-data-source data)
               (checkpoint-equal-p (attestation-data-source data) justified))
      ;; Source is timely if included within sqrt(SLOTS_PER_EPOCH) slots
      (when (<= inclusion-delay (integer-square-root +slots-per-epoch+))
        (push +timely-source-flag-index+ flags)))

    ;; Check target (epoch boundary block)
    (when (and (attestation-data-target data)
               (equalp (checkpoint-root (attestation-data-target data)) target-root))
      ;; Target is timely if included within SLOTS_PER_EPOCH slots
      (when (<= inclusion-delay +slots-per-epoch+)
        (push +timely-target-flag-index+ flags)))

    ;; Check head (correct head vote)
    (when (equalp (attestation-data-beacon-block-root data) head-root)
      ;; Head is timely if included in next slot
      (when (= inclusion-delay +min-attestation-inclusion-delay+)
        (push +timely-head-flag-index+ flags)))

    (nreverse flags)))

;;; ============================================================================
;;; Attestation Processing
;;; ============================================================================

(defun process-attestation (state attestation)
  "Process an attestation and update participation flags.
   Returns T on success, or signals an error on validation failure."
  ;; Validate
  (multiple-value-bind (valid-p error-msg)
      (validate-attestation state attestation)
    (unless valid-p
      (error "Invalid attestation: ~A" error-msg)))

  ;; Get participation flags
  (let* ((data (attestation-data attestation))
         (att-slot (attestation-data-slot data))
         (inclusion-delay (- (beacon-state-slot state) att-slot))
         (participation-flags (get-attestation-participation-flags state attestation inclusion-delay))
         (attesting-indices (get-attesting-indices state attestation))
         (att-epoch (compute-epoch-at-slot att-slot))
         (epoch-type (if (= att-epoch (get-current-epoch state)) :current :previous)))

    ;; Update participation for each attesting validator
    (dolist (idx attesting-indices)
      (dolist (flag participation-flags)
        (set-participation-flag state idx flag epoch-type)))

    ;; Update proposer reward
    (let* ((proposer-index (get-beacon-proposer-index state))
           (total-active-balance (get-total-active-balance state))
           (attesting-balance (get-total-balance state attesting-indices))
           (base-reward (compute-base-reward state (car attesting-indices))))
      (when (and proposer-index (> base-reward 0))
        (let ((proposer-reward
                (floor (* (floor (* base-reward (length participation-flags) +proposer-weight+)
                                 +weight-denominator+)
                          attesting-balance)
                       total-active-balance)))
          (increase-balance state proposer-index proposer-reward))))

    t))

;;; ============================================================================
;;; Reward Computation Helpers
;;; ============================================================================

(defun compute-base-reward (state index)
  "Compute base reward for a validator."
  (let* ((total-balance (get-total-active-balance state))
         (sqrt-balance (integer-square-root total-balance))
         (effective-balance (validator-effective-balance (get-validator state index))))
    (if (zerop sqrt-balance)
        0
        (floor (* effective-balance +base-reward-factor+)
               (* sqrt-balance +weight-denominator+)))))

(defun compute-base-reward-per-increment (state)
  "Compute base reward per balance increment."
  (let* ((total-balance (get-total-active-balance state))
         (sqrt-balance (integer-square-root total-balance)))
    (if (zerop sqrt-balance)
        0
        (floor (* +effective-balance-increment+ +base-reward-factor+)
               sqrt-balance))))
