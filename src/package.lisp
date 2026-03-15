;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:cl-beacon-state-machine
  (:use #:cl)
  (:export
   #:identity-list
   #:flatten
   #:map-keys
   #:now-timestamp
#:with-beacon-state-machine-timing
   #:beacon-state-machine-batch-process
   #:beacon-state-machine-health-check#:cl-beacon-state-machine-error
   #:cl-beacon-state-machine-validation-error#:init
   #:status
   #:cleanup
   #:process
   #:validate))
