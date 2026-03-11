;;;; util.lisp
;;;;
;;;; Utility functions and inlined crypto for cl-beacon-state-machine
;;;; Includes SHA-256 and BLS stub implementations
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:cl-beacon-state-machine)

;;; ============================================================================
;;; Optimized SHA-256 Implementation (Inlined)
;;; ============================================================================

(declaim (optimize (speed 3) (safety 1) (debug 0)))

;;; SHA-256 Constants
(defparameter +sha256-k+
  (make-array 64 :element-type '(unsigned-byte 32)
              :initial-contents
              '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
                #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                #xd807aa98 #x12835b01 #x243185be #x550c7dc3
                #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
                #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
                #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
                #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
                #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
                #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                #x748f82ee #x78a5636f #x84c87814 #x8cc70208
                #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

(defmacro u32 (x)
  "Truncate to 32 bits."
  `(logand #xFFFFFFFF ,x))

(declaim (inline u32+ u32-rotr))

(defun u32+ (&rest args)
  "32-bit addition with overflow."
  (u32 (apply #'+ args)))

(defun u32-rotr (x n)
  "32-bit right rotation."
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 31) n))
  (logior (ash x (- n))
          (u32 (ash x (- 32 n)))))

;;; SHA-256 round functions
(declaim (inline sha256-ch sha256-maj sha256-sigma0 sha256-sigma1 sha256-gamma0 sha256-gamma1))

(defun sha256-ch (x y z)
  (declare (type (unsigned-byte 32) x y z))
  (logxor (logand x y) (logand (lognot x) z)))

(defun sha256-maj (x y z)
  (declare (type (unsigned-byte 32) x y z))
  (logxor (logand x y) (logand x z) (logand y z)))

(defun sha256-sigma0 (x)
  (declare (type (unsigned-byte 32) x))
  (logxor (u32-rotr x 2) (u32-rotr x 13) (u32-rotr x 22)))

(defun sha256-sigma1 (x)
  (declare (type (unsigned-byte 32) x))
  (logxor (u32-rotr x 6) (u32-rotr x 11) (u32-rotr x 25)))

(defun sha256-gamma0 (x)
  (declare (type (unsigned-byte 32) x))
  (logxor (u32-rotr x 7) (u32-rotr x 18) (ash x -3)))

(defun sha256-gamma1 (x)
  (declare (type (unsigned-byte 32) x))
  (logxor (u32-rotr x 17) (u32-rotr x 19) (ash x -10)))

(defun sha256-transform (h block-data)
  "Process a single 64-byte block."
  (declare (type (simple-array (unsigned-byte 32) (8)) h)
           (type (simple-array (unsigned-byte 8) (*)) block-data))
  (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; Copy hash state
    (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
          (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
      (declare (type (unsigned-byte 32) a b c d e f g hh))

      ;; Prepare message schedule W[0..15]
      (loop for i from 0 below 16
            for j = (* i 4) do
        (setf (aref w i)
              (logior (ash (aref block-data j) 24)
                      (ash (aref block-data (+ j 1)) 16)
                      (ash (aref block-data (+ j 2)) 8)
                      (aref block-data (+ j 3)))))

      ;; Expand W[16..63]
      (loop for i from 16 below 64 do
        (setf (aref w i)
              (u32+ (aref w (- i 16))
                    (sha256-gamma0 (aref w (- i 15)))
                    (aref w (- i 7))
                    (sha256-gamma1 (aref w (- i 2))))))

      ;; 64 rounds of compression
      (loop for i from 0 below 64 do
        (let* ((temp1 (u32+ hh
                            (sha256-sigma1 e)
                            (sha256-ch e f g)
                            (aref +sha256-k+ i)
                            (aref w i)))
               (temp2 (u32+ (sha256-sigma0 a)
                            (sha256-maj a b c))))
          (setf hh g g f f e e (u32+ d temp1)
                d c c b b a a (u32+ temp1 temp2))))

      ;; Add to hash
      (setf (aref h 0) (u32+ (aref h 0) a)
            (aref h 1) (u32+ (aref h 1) b)
            (aref h 2) (u32+ (aref h 2) c)
            (aref h 3) (u32+ (aref h 3) d)
            (aref h 4) (u32+ (aref h 4) e)
            (aref h 5) (u32+ (aref h 5) f)
            (aref h 6) (u32+ (aref h 6) g)
            (aref h 7) (u32+ (aref h 7) hh)))))

(defun sha256 (data)
  "Compute SHA-256 hash of DATA. Returns 32-byte array."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((h (make-array 8 :element-type '(unsigned-byte 32)
                       :initial-contents '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                          #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
        (total-bytes (length data))
        (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
        (buffer-index 0))
    (declare (type (simple-array (unsigned-byte 32) (8)) h)
             (type (simple-array (unsigned-byte 8) (64)) buffer)
             (type fixnum buffer-index total-bytes))

    ;; Process full blocks
    (loop for i from 0 below (length data) do
      (setf (aref buffer buffer-index) (aref data i))
      (incf buffer-index)
      (when (= buffer-index 64)
        (sha256-transform h buffer)
        (setf buffer-index 0)))

    ;; Pad
    (setf (aref buffer buffer-index) #x80)
    (incf buffer-index)

    (when (> buffer-index 56)
      (loop for i from buffer-index below 64 do
        (setf (aref buffer i) 0))
      (sha256-transform h buffer)
      (setf buffer-index 0))

    (loop for i from buffer-index below 56 do
      (setf (aref buffer i) 0))

    ;; Append length (big-endian, in bits)
    (let ((bit-len (* total-bytes 8)))
      (setf (aref buffer 56) (ldb (byte 8 56) bit-len)
            (aref buffer 57) (ldb (byte 8 48) bit-len)
            (aref buffer 58) (ldb (byte 8 40) bit-len)
            (aref buffer 59) (ldb (byte 8 32) bit-len)
            (aref buffer 60) (ldb (byte 8 24) bit-len)
            (aref buffer 61) (ldb (byte 8 16) bit-len)
            (aref buffer 62) (ldb (byte 8 8) bit-len)
            (aref buffer 63) (ldb (byte 8 0) bit-len)))

    (sha256-transform h buffer)

    ;; Output (big-endian)
    (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for i from 0 below 8
            for hi = (aref h i) do
        (setf (aref out (* i 4)) (ldb (byte 8 24) hi)
              (aref out (+ (* i 4) 1)) (ldb (byte 8 16) hi)
              (aref out (+ (* i 4) 2)) (ldb (byte 8 8) hi)
              (aref out (+ (* i 4) 3)) (ldb (byte 8 0) hi)))
      out)))

;;; ============================================================================
;;; BLS Signature Stubs
;;; ============================================================================
;;; These are stub implementations for the BLS12-381 signature scheme.
;;; In production, replace with actual cryptographic implementation.

(defun bls-verify (pubkey message signature)
  "Verify a BLS signature. STUB: Always returns T.
   PUBKEY: 48-byte public key
   MESSAGE: Arbitrary byte array
   SIGNATURE: 96-byte signature
   Returns: T if valid, NIL otherwise"
  (declare (ignore pubkey message signature))
  ;; STUB: In production, implement actual BLS verification
  t)

(defun bls-aggregate (signatures)
  "Aggregate multiple BLS signatures. STUB.
   SIGNATURES: List of 96-byte signatures
   Returns: 96-byte aggregate signature"
  (declare (ignore signatures))
  ;; STUB: Return dummy signature
  (make-array 96 :element-type '(unsigned-byte 8) :initial-element 0))

(defun bls-aggregate-verify (pubkeys messages signature)
  "Verify an aggregate BLS signature. STUB: Always returns T.
   PUBKEYS: List of 48-byte public keys
   MESSAGES: List of message byte arrays
   SIGNATURE: 96-byte aggregate signature
   Returns: T if valid, NIL otherwise"
  (declare (ignore pubkeys messages signature))
  ;; STUB: In production, implement actual aggregate verification
  t)

;;; ============================================================================
;;; Byte Array Utilities
;;; ============================================================================

(defun integer-square-root (n)
  "Compute integer square root using Newton's method."
  (declare (type (integer 0) n))
  (if (<= n 1)
      n
      (let ((x n))
        (loop for y = (floor (+ x (floor n x)) 2)
              while (< y x)
              do (setf x y)
              finally (return x)))))

(defun xor-bytes (a b)
  "XOR two byte arrays of the same length."
  (declare (type (simple-array (unsigned-byte 8) (*)) a b))
  (let ((result (make-array (length a) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length a)
          do (setf (aref result i) (logxor (aref a i) (aref b i))))
    result))

(defun bytes-to-integer (bytes)
  "Convert byte array to integer (little-endian)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (loop for i from 0 below (length bytes)
        sum (ash (aref bytes i) (* 8 i))))

(defun integer-to-bytes (n length)
  "Convert integer to byte array (little-endian)."
  (declare (type integer n)
           (type fixnum length))
  (let ((result (make-array length :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 0 below length
          do (setf (aref result i) (ldb (byte 8 (* i 8)) n)))
    result))

(defun concat-bytes (&rest arrays)
  "Concatenate byte arrays."
  (let* ((total-length (reduce #'+ arrays :key #'length))
         (result (make-array total-length :element-type '(unsigned-byte 8)))
         (pos 0))
    (dolist (arr arrays result)
      (replace result arr :start1 pos)
      (incf pos (length arr)))))
