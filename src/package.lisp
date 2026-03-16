;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:cl-beacon-state-machine
  (:use #:cl)
  (:export
   #:beacon-state-machine-execute
   #:initialize-beacon-state-machine
   #:beacon-state-machine-context
   #:memoize-function
   #:deep-copy-list
   #:group-by-count
   #:identity-list
   #:flatten
   #:map-keys
   #:now-timestamp
   #:with-beacon-state-machine-timing
   #:beacon-state-machine-batch-process
   #:beacon-state-machine-health-check
   #:cl-beacon-state-machine-error
   #:cl-beacon-state-machine-validation-error
   #:init
   #:status
   #:cleanup
   #:process
   #:validate
   #:initialize-beacon-state
   #:add-validator
   #:increase-balance
   #:decrease-balance
   #:set-validator-status
   #:activate-validator
   #:request-validator-exit
   #:slash-validator
   #:advance-epoch
   #:get-validator-count
   #:get-active-validator-count
   #:get-total-active-balance
   #:update-randao
   #:get-randao-mix
   #:update-justification
   #:update-finalization
   #:beacon-state
   #:beacon-state-p
   #:beacon-state-genesis-time
   #:beacon-state-slot
   #:beacon-state-validators
   #:beacon-state-balances
   #:beacon-state-randao-mixes
   #:beacon-state-finalized-checkpoint
   #:validator-record
   #:validator-record-p
   #:validator-record-pubkey
   #:validator-record-status
   #:validator-record-balance))
