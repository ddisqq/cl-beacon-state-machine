;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-beacon-state-machine)

(define-condition cl-beacon-state-machine-error (error)
  ((message :initarg :message :reader cl-beacon-state-machine-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-beacon-state-machine error: ~A" (cl-beacon-state-machine-error-message condition)))))
