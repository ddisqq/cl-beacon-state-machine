# cl-beacon-state-machine

Pure Common Lisp implementation of Beacon State Machine

## Overview
This library provides a robust, zero-dependency implementation of Beacon State Machine for the Common Lisp ecosystem. It is designed to be highly portable, performant, and easy to integrate into any SBCL/CCL/ECL environment.

## Getting Started

Load the system using ASDF:

```lisp
(asdf:load-system #:cl-beacon-state-machine)
```

## Usage Example

```lisp
;; Initialize the environment
(let ((ctx (cl-beacon-state-machine:initialize-beacon-state-machine :initial-id 42)))
  ;; Perform batch processing using the built-in standard toolkit
  (multiple-value-bind (results errors)
      (cl-beacon-state-machine:beacon-state-machine-batch-process '(1 2 3) #'identity)
    (format t "Processed ~A items with ~A errors.~%" (length results) (length errors))))
```

## License
Apache-2.0
