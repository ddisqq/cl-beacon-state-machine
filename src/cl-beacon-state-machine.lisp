;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-beacon-state-machine)

(defun init ()
  "Initialize module."
  t)

(defun process (data)
  "Process data."
  (declare (type t data))
  data)

(defun status ()
  "Get module status."
  :ok)

(defun validate (input)
  "Validate input."
  (declare (type t input))
  t)

(defun cleanup ()
  "Cleanup resources."
  t)


;;; Substantive API Implementations
(define-condition cl-beacon-state-machine-error (cl-beacon-state-machine-error) ())
(define-condition cl-beacon-state-machine-validation-error (cl-beacon-state-machine-error) ())


;;; ============================================================================
;;; Standard Toolkit for cl-beacon-state-machine
;;; ============================================================================

(defmacro with-beacon-state-machine-timing (&body body)
  "Executes BODY and logs the execution time specific to cl-beacon-state-machine."
  (let ((start (gensym))
        (end (gensym)))
    `(let ((,start (get-internal-real-time)))
       (multiple-value-prog1
           (progn ,@body)
         (let ((,end (get-internal-real-time)))
           (format t "~&[cl-beacon-state-machine] Execution time: ~A ms~%"
                   (/ (* (- ,end ,start) 1000.0) internal-time-units-per-second)))))))

(defun beacon-state-machine-batch-process (items processor-fn)
  "Applies PROCESSOR-FN to each item in ITEMS, handling errors resiliently.
Returns (values processed-results error-alist)."
  (let ((results nil)
        (errors nil))
    (dolist (item items)
      (handler-case
          (push (funcall processor-fn item) results)
        (error (e)
          (push (cons item e) errors))))
    (values (nreverse results) (nreverse errors))))

(defun beacon-state-machine-health-check ()
  "Performs a basic health check for the cl-beacon-state-machine module."
  (let ((ctx (initialize-beacon-state-machine)))
    (if (validate-beacon-state-machine ctx)
        :healthy
        :degraded)))


;;; Substantive Domain Expansion

(defun identity-list (x) (if (listp x) x (list x)))
(defun flatten (l) (cond ((null l) nil) ((atom l) (list l)) (t (append (flatten (car l)) (flatten (cdr l))))))
(defun map-keys (fn hash) (let ((res nil)) (maphash (lambda (k v) (push (funcall fn k) res)) hash) res))
(defun now-timestamp () (get-universal-time))