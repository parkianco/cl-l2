;;;; cl-l2/src/util.lisp - Utility Functions
;;;; Pure Common Lisp utilities for L2 primitives
;;;;
;;;; Zero external dependencies - SBCL-specific threading only

(in-package #:cl-l2)

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconstant +default-tree-depth+ 32
  "Default depth for sparse Merkle trees (supports 2^32 accounts).")

(defconstant +empty-leaf-hash+ 0
  "Hash value representing an empty leaf.")

(defconstant +batch-target-size+ 512
  "Target number of transactions per batch.")

(defconstant +batch-max-size+ 2048
  "Maximum transactions per batch.")

(defconstant +batch-timeout+ 60
  "Maximum seconds to wait for batch to fill.")

(defconstant +max-mempool-size+ 100000
  "Maximum transactions in mempool.")

(defconstant +commitment-finality-blocks+ 64
  "L1 blocks required for commitment finality.")

;;; ============================================================================
;;; Batch Status Constants
;;; ============================================================================

(defconstant +batch-status-pending+ :pending)
(defconstant +batch-status-proving+ :proving)
(defconstant +batch-status-proved+ :proved)
(defconstant +batch-status-submitted+ :submitted)
(defconstant +batch-status-confirmed+ :confirmed)
(defconstant +batch-status-finalized+ :finalized)
(defconstant +batch-status-rejected+ :rejected)

;;; ============================================================================
;;; Rollup Status Constants
;;; ============================================================================

(defconstant +rollup-status-initializing+ :initializing)
(defconstant +rollup-status-running+ :running)
(defconstant +rollup-status-paused+ :paused)
(defconstant +rollup-status-stopped+ :stopped)

;;; ============================================================================
;;; Hash Functions (Simplified Implementations)
;;; ============================================================================

;; Note: These are simplified hash implementations for demonstration.
;; Production use should employ proper cryptographic implementations.

(defun poseidon-hash (&rest inputs)
  "Simplified Poseidon hash (placeholder).
   In production, use proper field arithmetic and round constants."
  (let ((result 0))
    (dolist (input inputs)
      (setf result (logxor result (mod (* input 31) (ash 1 256)))))
    (mod (+ result (ash 1 64)) (ash 1 256))))

(defun poseidon-hash-2 (a b)
  "Hash two field elements."
  (poseidon-hash a b))

(defun poseidon-hash-4 (a b c d)
  "Hash four field elements."
  (poseidon-hash a b c d))

(defun sha256 (data)
  "Simplified SHA256 placeholder.
   DATA should be a byte vector or integer."
  (let ((input (if (integerp data) data
                   (bytes-to-integer data))))
    ;; Simplified mixing (not cryptographically secure)
    (let ((h #x6a09e667bb67ae85))
      (setf h (logxor h input))
      (setf h (mod (* h #x5851f42d4c957f2d) (ash 1 256)))
      (setf h (logxor h (ash h -33)))
      h)))

(defun keccak256 (data)
  "Simplified Keccak256 placeholder."
  (let ((input (if (integerp data) data
                   (bytes-to-integer data))))
    (let ((h #x1c))
      (setf h (logxor h input))
      (setf h (mod (* h #x517cc1b727220a95) (ash 1 256)))
      (setf h (logxor h (ash h -37)))
      h)))

;;; ============================================================================
;;; Byte/Integer Conversion
;;; ============================================================================

(defun bytes-to-integer (bytes)
  "Convert byte vector to integer (big-endian)."
  (let ((result 0))
    (loop for byte across bytes
          do (setf result (logior (ash result 8) byte)))
    result))

(defun integer-to-bytes (n &optional (size 32))
  "Convert integer to byte vector (big-endian)."
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (loop for i from (1- size) downto 0
          for shift from 0 by 8
          do (setf (aref bytes i) (ldb (byte 8 shift) n)))
    bytes))

(defun hex-encode (bytes)
  "Encode byte vector as hexadecimal string."
  (with-output-to-string (s)
    (loop for byte across bytes
          do (format s "~2,'0X" byte))))

(defun hex-decode (hex-string)
  "Decode hexadecimal string to byte vector."
  (let* ((str (if (and (>= (length hex-string) 2)
                       (string= (subseq hex-string 0 2) "0x"))
                  (subseq hex-string 2)
                  hex-string))
         (len (floor (length str) 2))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          do (setf (aref bytes i)
                   (parse-integer str :start (* i 2) :end (+ (* i 2) 2) :radix 16)))
    bytes))

;;; ============================================================================
;;; Threading Utilities (SBCL-specific)
;;; ============================================================================

(defun make-lock (&optional name)
  "Create a mutex lock."
  #+sbcl (sb-thread:make-mutex :name (or name "cl-l2-lock"))
  #-sbcl nil)

(defmacro with-lock ((lock) &body body)
  "Execute BODY with exclusive access via LOCK."
  #+sbcl
  `(if ,lock
       (sb-thread:with-mutex (,lock)
         ,@body)
       (progn ,@body))
  #-sbcl
  `(progn ,@body))

;;; ============================================================================
;;; Hash Table Utilities
;;; ============================================================================

(defun copy-hash-table (ht)
  "Create a shallow copy of a hash table."
  (let ((new-ht (make-hash-table
                 :test (hash-table-test ht)
                 :size (hash-table-size ht))))
    (maphash (lambda (k v) (setf (gethash k new-ht) v)) ht)
    new-ht))

;;; ============================================================================
;;; Time Utilities
;;; ============================================================================

(defun current-timestamp ()
  "Get current Unix timestamp."
  (- (get-universal-time) 2208988800))  ; Convert CL time to Unix epoch

;;; ============================================================================
;;; Serialization (Compact Binary Format)
;;; ============================================================================

(defun write-varint (n stream)
  "Write variable-length integer to stream."
  (loop while (>= n #x80)
        do (write-byte (logior (logand n #x7f) #x80) stream)
           (setf n (ash n -7)))
  (write-byte n stream))

(defun read-varint (stream)
  "Read variable-length integer from stream."
  (let ((result 0)
        (shift 0))
    (loop for byte = (read-byte stream)
          do (setf result (logior result (ash (logand byte #x7f) shift)))
             (incf shift 7)
          until (zerop (logand byte #x80)))
    result))

(defun write-bytes (bytes stream)
  "Write length-prefixed bytes to stream."
  (write-varint (length bytes) stream)
  (write-sequence bytes stream))

(defun read-bytes (stream)
  "Read length-prefixed bytes from stream."
  (let* ((len (read-varint stream))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    bytes))

;;; ============================================================================
;;; Validation Utilities
;;; ============================================================================

(defun valid-address-p (addr)
  "Check if ADDR is a valid address (non-negative integer)."
  (and (integerp addr) (>= addr 0)))

(defun valid-amount-p (amount)
  "Check if AMOUNT is a valid transfer amount."
  (and (integerp amount) (> amount 0)))

(defun valid-nonce-p (nonce)
  "Check if NONCE is valid."
  (and (integerp nonce) (>= nonce 0)))
