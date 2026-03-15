;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:cl-beacon-state-machine
  (:use #:cl)
  (:export
   #:init
   #:status
   #:cleanup
   #:process
   #:validate))
