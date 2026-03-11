;;;; package.lisp
;;;;
;;;; Package definition for cl-beacon-state-machine
;;;; A standalone PoS beacon chain state machine
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(defpackage #:cl-beacon-state-machine
  (:use #:cl)
  (:nicknames #:beacon-sm)
  (:documentation "Proof-of-Stake beacon chain state machine.
Implements the Ethereum 2.0 beacon state specification with
epoch processing, validator management, and attestation handling.")
  (:export
   ;; =========================================================================
   ;; Constants - Time Parameters
   ;; =========================================================================
   #:+seconds-per-slot+
   #:+slots-per-epoch+
   #:+min-seed-lookahead+
   #:+max-seed-lookahead+
   #:+epochs-per-eth1-voting-period+
   #:+slots-per-historical-root+
   #:+epochs-per-historical-vector+
   #:+epochs-per-slashings-vector+
   #:+min-epochs-to-inactivity-penalty+
   #:+shard-committee-period+

   ;; =========================================================================
   ;; Constants - Validator
   ;; =========================================================================
   #:+max-committees-per-slot+
   #:+target-committee-size+
   #:+max-validators-per-committee+
   #:+shuffle-round-count+
   #:+min-deposit-amount+
   #:+max-effective-balance+
   #:+effective-balance-increment+
   #:+min-attestation-inclusion-delay+
   #:+far-future-epoch+

   ;; =========================================================================
   ;; Constants - Rewards
   ;; =========================================================================
   #:+proposer-reward-quotient+
   #:+inactivity-penalty-quotient+
   #:+min-slashing-penalty-quotient+
   #:+whistleblower-reward-quotient+
   #:+base-reward-factor+
   #:+weight-denominator+
   #:+timely-source-weight+
   #:+timely-target-weight+
   #:+timely-head-weight+
   #:+proposer-weight+

   ;; =========================================================================
   ;; Crypto Utilities
   ;; =========================================================================
   #:sha256
   #:bls-verify
   #:bls-aggregate
   #:bls-aggregate-verify

   ;; =========================================================================
   ;; Core Types - Checkpoint
   ;; =========================================================================
   #:checkpoint
   #:make-checkpoint
   #:checkpoint-p
   #:checkpoint-epoch
   #:checkpoint-root
   #:checkpoint-equal-p

   ;; =========================================================================
   ;; Core Types - Fork
   ;; =========================================================================
   #:fork
   #:make-fork
   #:fork-p
   #:fork-previous-version
   #:fork-current-version
   #:fork-epoch

   ;; =========================================================================
   ;; Core Types - Validator
   ;; =========================================================================
   #:validator
   #:make-validator
   #:validator-p
   #:validator-pubkey
   #:validator-withdrawal-credentials
   #:validator-effective-balance
   #:validator-slashed
   #:validator-activation-eligibility-epoch
   #:validator-activation-epoch
   #:validator-exit-epoch
   #:validator-withdrawable-epoch
   #:validator-is-active-p
   #:validator-is-slashable-p

   ;; =========================================================================
   ;; Core Types - Attestation
   ;; =========================================================================
   #:attestation-data
   #:make-attestation-data
   #:attestation-data-p
   #:attestation-data-slot
   #:attestation-data-index
   #:attestation-data-beacon-block-root
   #:attestation-data-source
   #:attestation-data-target
   #:attestation-data-equal-p

   #:attestation
   #:make-attestation
   #:attestation-p
   #:attestation-aggregation-bits
   #:attestation-data
   #:attestation-signature

   ;; =========================================================================
   ;; Core Types - Eth1Data
   ;; =========================================================================
   #:eth1-data
   #:make-eth1-data
   #:eth1-data-p
   #:eth1-data-deposit-root
   #:eth1-data-deposit-count
   #:eth1-data-block-hash

   ;; =========================================================================
   ;; BeaconState
   ;; =========================================================================
   #:beacon-state
   #:make-beacon-state
   #:beacon-state-p
   #:beacon-state-genesis-time
   #:beacon-state-genesis-validators-root
   #:beacon-state-slot
   #:beacon-state-fork
   #:beacon-state-validators
   #:beacon-state-balances
   #:beacon-state-randao-mixes
   #:beacon-state-slashings
   #:beacon-state-previous-epoch-participation
   #:beacon-state-current-epoch-participation
   #:beacon-state-justification-bits
   #:beacon-state-previous-justified-checkpoint
   #:beacon-state-current-justified-checkpoint
   #:beacon-state-finalized-checkpoint
   #:beacon-state-inactivity-scores
   #:beacon-state-eth1-data
   #:beacon-state-eth1-data-votes
   #:beacon-state-eth1-deposit-index
   #:beacon-state-block-roots
   #:beacon-state-state-roots

   ;; =========================================================================
   ;; State Accessors
   ;; =========================================================================
   #:get-current-epoch
   #:get-previous-epoch
   #:get-next-epoch
   #:compute-epoch-at-slot
   #:compute-start-slot-at-epoch
   #:is-epoch-boundary-p
   #:get-active-validator-indices
   #:get-active-validator-count
   #:get-total-active-balance
   #:get-validator-churn-limit
   #:get-beacon-proposer-index
   #:get-beacon-committee
   #:get-randao-mix
   #:get-block-root
   #:get-block-root-at-slot
   #:get-finalized-epoch
   #:get-justified-epoch
   #:get-finality-delay

   ;; =========================================================================
   ;; State Mutators
   ;; =========================================================================
   #:add-validator
   #:slash-validator
   #:initiate-validator-exit
   #:increase-balance
   #:decrease-balance
   #:set-participation-flag
   #:rotate-participation

   ;; =========================================================================
   ;; State Transition
   ;; =========================================================================
   #:state-transition
   #:process-slots
   #:process-slot
   #:process-epoch
   #:process-block
   #:process-attestation

   ;; =========================================================================
   ;; Epoch Processing
   ;; =========================================================================
   #:process-justification-and-finalization
   #:process-inactivity-updates
   #:process-rewards-and-penalties
   #:process-registry-updates
   #:process-slashings
   #:process-effective-balance-updates
   #:process-participation-flag-updates

   ;; =========================================================================
   ;; Attestation Processing
   ;; =========================================================================
   #:validate-attestation
   #:get-attesting-indices
   #:get-attestation-participation-flags

   ;; =========================================================================
   ;; Shuffling
   ;; =========================================================================
   #:compute-shuffled-index
   #:compute-proposer-index
   #:compute-committee

   ;; =========================================================================
   ;; Utilities
   ;; =========================================================================
   #:integer-square-root
   #:xor-bytes
   #:bytes-to-integer
   #:integer-to-bytes
   #:concat-bytes))

(defpackage #:cl-beacon-state-machine.test
  (:use #:cl #:cl-beacon-state-machine)
  (:export #:run-tests))
