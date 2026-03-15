;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-beacon-state-machine)

;;; Core types for cl-beacon-state-machine
(deftype cl-beacon-state-machine-id () '(unsigned-byte 64))
(deftype cl-beacon-state-machine-status () '(member :ready :active :error :shutdown))
