;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-l2/src/commitment.lisp - State Commitments
;;;; Pure Common Lisp state commitment and proof management
;;;;
;;;; Zero external dependencies - SBCL-specific threading only

(in-package #:cl-l2)

;;; ============================================================================
;;; State Commitment Structure
;;; ============================================================================

(defstruct (state-commitment (:constructor %make-state-commitment))
  "A commitment to L2 state posted to L1."
  (batch-id 0 :type integer)
  (state-root 0 :type integer)
  (timestamp 0 :type integer)
  (proof nil)  ; ZK proof or validity proof
  (l1-block nil :type (or null integer))
  (l1-tx-hash nil :type (or null integer))
  (finalized-p nil :type boolean))

(defun make-state-commitment (&key batch-id state-root proof)
  "Create a new state commitment."
  (%make-state-commitment
   :batch-id (or batch-id 0)
   :state-root (or state-root 0)
   :timestamp (current-timestamp)
   :proof proof
   :l1-block nil
   :l1-tx-hash nil
   :finalized-p nil))

;;; ============================================================================
;;; Commitment Operations
;;; ============================================================================

(defun create-commitment (batch state-db)
  "Create a state commitment from a finalized batch."
  (let ((commitment (make-state-commitment
                     :batch-id (l2-batch-id batch)
                     :state-root (get-current-root state-db)
                     :proof nil)))
    commitment))

(defun verify-commitment (commitment state-db)
  "Verify that a commitment matches the current state."
  (and commitment
       (= (state-commitment-state-root commitment)
          (get-current-root state-db))))

(defun finalize-commitment (commitment l1-block l1-tx-hash)
  "Mark a commitment as finalized on L1."
  (setf (state-commitment-l1-block commitment) l1-block
        (state-commitment-l1-tx-hash commitment) l1-tx-hash
        (state-commitment-finalized-p commitment) t)
  commitment)

;;; ============================================================================
;;; Commitment Registry
;;; ============================================================================

(defstruct (commitment-registry (:constructor %make-commitment-registry))
  "Registry for tracking state commitments."
  (commitments (make-hash-table) :type hash-table)
  (by-batch (make-hash-table) :type hash-table)
  (pending (list) :type list)
  (latest-id 0 :type integer)
  (latest-finalized-id nil :type (or null integer))
  (lock (make-lock "commitment-registry")))

(defun make-commitment-registry ()
  "Create a new commitment registry."
  (%make-commitment-registry))

(defun register-commitment (registry commitment)
  "Register a new commitment."
  (with-lock ((commitment-registry-lock registry))
    (let ((id (incf (commitment-registry-latest-id registry))))
      (setf (gethash id (commitment-registry-commitments registry)) commitment)
      (setf (gethash (state-commitment-batch-id commitment)
                     (commitment-registry-by-batch registry))
            id)
      (push id (commitment-registry-pending registry))
      id)))

(defun get-commitment (registry id)
  "Get a commitment by ID."
  (gethash id (commitment-registry-commitments registry)))

(defun get-commitment-by-batch (registry batch-id)
  "Get a commitment by batch ID."
  (let ((id (gethash batch-id (commitment-registry-by-batch registry))))
    (when id
      (get-commitment registry id))))

(defun get-commitments-in-range (registry start-id end-id)
  "Get all commitments in a range of IDs."
  (let ((result nil))
    (loop for id from start-id to end-id
          for commitment = (get-commitment registry id)
          when commitment
            do (push commitment result))
    (nreverse result)))

(defun get-latest-commitment (registry)
  "Get the most recent commitment."
  (get-commitment registry (commitment-registry-latest-id registry)))

(defun get-pending-commitments (registry)
  "Get all pending (unfinalized) commitments."
  (with-lock ((commitment-registry-lock registry))
    (mapcar (lambda (id) (get-commitment registry id))
            (commitment-registry-pending registry))))

(defun mark-commitment-finalized (registry id l1-block l1-tx-hash)
  "Mark a commitment as finalized."
  (with-lock ((commitment-registry-lock registry))
    (let ((commitment (get-commitment registry id)))
      (when commitment
        (finalize-commitment commitment l1-block l1-tx-hash)
        (setf (commitment-registry-pending registry)
              (remove id (commitment-registry-pending registry)))
        (setf (commitment-registry-latest-finalized-id registry) id)
        commitment))))

;;; ============================================================================
;;; State Proof Generation
;;; ============================================================================

(defun generate-state-proof (state-db batch)
  "Generate a state transition proof for a batch.
   Returns a proof structure suitable for L1 verification."
  (let ((old-root (l2-batch-old-state-root batch))
        (new-root (l2-batch-new-state-root batch))
        (tx-count (l2-batch-size batch)))
    (list :type :state-transition
          :old-root old-root
          :new-root new-root
          :tx-count tx-count
          :batch-hash (compute-batch-hash batch)
          :timestamp (current-timestamp))))

(defun compute-batch-hash (batch)
  "Compute the hash of a batch for commitment."
  (poseidon-hash (l2-batch-id batch)
                 (l2-batch-old-state-root batch)
                 (l2-batch-new-state-root batch)
                 (l2-batch-size batch)))

(defun generate-account-proof (state-db address)
  "Generate a Merkle proof for an account's state."
  (let* ((tree (state-db-tree state-db))
         (proof (smt-prove tree address)))
    (list :type :account-proof
          :address address
          :state-root (smt-root tree)
          :proof proof
          :account (state-db-get state-db address))))

(defun generate-multi-proof (state-db addresses)
  "Generate proofs for multiple accounts."
  (mapcar (lambda (addr) (generate-account-proof state-db addr))
          addresses))

(defun verify-account-proof (proof expected-root)
  "Verify an account proof against an expected root."
  (let ((merkle-proof (getf proof :proof))
        (address (getf proof :address))
        (account (getf proof :account)))
    (when merkle-proof
      (let ((leaf-value (if account
                            (hash-account-state account)
                            +empty-leaf-hash+)))
        (merkle-proof-verify merkle-proof address leaf-value expected-root)))))

;;; ============================================================================
;;; Commitment Verification
;;; ============================================================================

(defstruct commitment-proof
  "Proof data for L1 commitment verification."
  (commitment nil :type (or null state-commitment))
  (state-proof nil :type list)
  (account-proofs nil :type list)
  (signature nil))

(defun create-commitment-proof (commitment state-db batch &optional touched-accounts)
  "Create a complete proof package for L1 submission."
  (make-commitment-proof
   :commitment commitment
   :state-proof (generate-state-proof state-db batch)
   :account-proofs (when touched-accounts
                     (generate-multi-proof state-db touched-accounts))
   :signature nil))

(defun serialize-commitment-proof (proof stream)
  "Serialize a commitment proof for L1 submission."
  (let ((commitment (commitment-proof-commitment proof)))
    ;; Batch ID
    (write-varint (state-commitment-batch-id commitment) stream)
    ;; State root
    (let ((root-bytes (integer-to-bytes (state-commitment-state-root commitment) 32)))
      (write-sequence root-bytes stream))
    ;; Timestamp
    (write-varint (state-commitment-timestamp commitment) stream)
    ;; State proof data
    (let* ((state-proof (commitment-proof-state-proof proof))
           (old-root (getf state-proof :old-root))
           (new-root (getf state-proof :new-root))
           (tx-count (getf state-proof :tx-count)))
      (write-sequence (integer-to-bytes old-root 32) stream)
      (write-sequence (integer-to-bytes new-root 32) stream)
      (write-varint tx-count stream))))

(defun commitment-calldata (proof)
  "Generate calldata for L1 contract submission."
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
    (with-output-to-string (s)
      (declare (ignore s))
      ;; This would generate actual calldata in production
      ;; For now, return a placeholder
      (let ((commitment (commitment-proof-commitment proof)))
        (vector-push-extend #x01 buffer)  ; Method selector placeholder
        (loop for byte across (integer-to-bytes (state-commitment-state-root commitment) 32)
              do (vector-push-extend byte buffer))))
    buffer))

;;; ============================================================================
;;; Finality Tracking
;;; ============================================================================

(defstruct finality-tracker
  "Tracks finality of commitments across L1 blocks."
  (registry nil :type (or null commitment-registry))
  (current-l1-block 0 :type integer)
  (finality-threshold +commitment-finality-blocks+ :type integer)
  (pending-finality (make-hash-table) :type hash-table))

;; Note: make-finality-tracker constructor is automatically generated by defstruct
;; Use it with keyword args: (make-finality-tracker :registry registry :finality-threshold threshold)

(defun track-commitment-submission (tracker commitment-id l1-block)
  "Track when a commitment was submitted to L1."
  (setf (gethash commitment-id (finality-tracker-pending-finality tracker))
        l1-block))

(defun update-l1-block (tracker l1-block)
  "Update the current L1 block and check for finalized commitments."
  (setf (finality-tracker-current-l1-block tracker) l1-block)
  (let ((registry (finality-tracker-registry tracker))
        (threshold (finality-tracker-finality-threshold tracker))
        (finalized nil))
    (maphash (lambda (commitment-id submit-block)
               (when (>= (- l1-block submit-block) threshold)
                 (mark-commitment-finalized registry commitment-id l1-block nil)
                 (push commitment-id finalized)))
             (finality-tracker-pending-finality tracker))
    ;; Remove finalized from pending
    (dolist (id finalized)
      (remhash id (finality-tracker-pending-finality tracker)))
    finalized))

(defun commitment-finality-status (tracker commitment-id)
  "Get the finality status of a commitment."
  (let* ((registry (finality-tracker-registry tracker))
         (commitment (get-commitment registry commitment-id)))
    (cond
      ((null commitment) :unknown)
      ((state-commitment-finalized-p commitment) :finalized)
      ((gethash commitment-id (finality-tracker-pending-finality tracker))
       :pending)
      (t :not-submitted))))

(defun blocks-until-finality (tracker commitment-id)
  "Calculate blocks remaining until finality."
  (let ((submit-block (gethash commitment-id
                                (finality-tracker-pending-finality tracker))))
    (if submit-block
        (max 0 (- (finality-tracker-finality-threshold tracker)
                  (- (finality-tracker-current-l1-block tracker) submit-block)))
        nil)))
