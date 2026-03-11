;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-l2/src/rollup.lisp - Rollup Coordination
;;;; Pure Common Lisp rollup coordination logic
;;;;
;;;; Zero external dependencies - SBCL-specific threading only

(in-package #:cl-l2)

;;; ============================================================================
;;; Rollup State
;;; ============================================================================

(defstruct (rollup-state (:constructor %make-rollup-state))
  "Complete state of an L2 rollup."
  (chain-id 0 :type integer)
  (current-batch nil)
  (state-db nil :type (or null state-db))
  (mempool nil :type (or null mempool))
  (batch-builder nil :type (or null batch-builder))
  (batch-tracker nil :type (or null batch-tracker))
  (commitment-registry nil :type (or null commitment-registry))
  (status +rollup-status-initializing+)
  (stats (make-hash-table) :type hash-table)
  (lock (make-lock "rollup-state")))

(defun make-rollup-state (&key chain-id)
  "Create a new rollup state."
  (%make-rollup-state
   :chain-id (or chain-id 1)
   :current-batch nil
   :state-db (make-state-db)
   :mempool (make-mempool)
   :batch-builder (make-batch-builder)
   :batch-tracker (make-batch-tracker)
   :commitment-registry (make-commitment-registry)
   :status +rollup-status-initializing+
   :stats (make-hash-table)))

;;; ============================================================================
;;; Rollup Lifecycle
;;; ============================================================================

(defun initialize-rollup (&key chain-id initial-state)
  "Initialize a new rollup instance."
  (let ((rollup (make-rollup-state :chain-id chain-id)))
    ;; Apply initial state if provided
    (when initial-state
      (maphash (lambda (addr account)
                 (state-db-put (rollup-state-state-db rollup) addr account))
               initial-state))
    ;; Initialize stats
    (setf (gethash :tx-count (rollup-state-stats rollup)) 0)
    (setf (gethash :batch-count (rollup-state-stats rollup)) 0)
    (setf (gethash :start-time (rollup-state-stats rollup)) (current-timestamp))
    rollup))

(defun start-rollup (rollup)
  "Start the rollup (mark as running)."
  (with-lock ((rollup-state-lock rollup))
    (setf (rollup-state-status rollup) +rollup-status-running+))
  rollup)

(defun stop-rollup (rollup)
  "Stop the rollup."
  (with-lock ((rollup-state-lock rollup))
    (setf (rollup-state-status rollup) +rollup-status-stopped+))
  rollup)

(defun pause-rollup (rollup)
  "Pause the rollup."
  (with-lock ((rollup-state-lock rollup))
    (setf (rollup-state-status rollup) +rollup-status-paused+))
  rollup)

(defun rollup-status (rollup)
  "Get the current rollup status."
  (rollup-state-status rollup))

(defun rollup-running-p (rollup)
  "Check if the rollup is running."
  (eq (rollup-state-status rollup) +rollup-status-running+))

;;; ============================================================================
;;; Transaction Processing
;;; ============================================================================

(defun submit-transaction (rollup tx)
  "Submit a transaction to the rollup mempool."
  (unless (rollup-running-p rollup)
    (return-from submit-transaction (values nil :rollup-not-running)))

  (let ((state-db (rollup-state-state-db rollup))
        (mempool (rollup-state-mempool rollup)))
    ;; Validate transaction
    (multiple-value-bind (valid-p error)
        (validate-transaction tx state-db)
      (unless valid-p
        (return-from submit-transaction (values nil error)))

      ;; Add to mempool
      (let ((added (mempool-add mempool tx)))
        (when added
          (incf (gethash :tx-count (rollup-state-stats rollup) 0)))
        (values added nil)))))

(defun apply-transaction (rollup tx)
  "Apply a single transaction to the rollup state."
  (let ((state-db (rollup-state-state-db rollup)))
    (multiple-value-bind (valid-p error)
        (validate-transaction tx state-db)
      (unless valid-p
        (return-from apply-transaction (values nil error)))

      ;; Execute based on transaction type
      (case (l2-tx-type tx)
        (:transfer (execute-transfer state-db tx))
        (:deposit (execute-deposit state-db tx))
        (:withdraw (execute-withdraw state-db tx))
        (otherwise (values nil :unknown-tx-type))))))

(defun execute-transfer (state-db tx)
  "Execute a transfer transaction."
  (let* ((from (l2-tx-from tx))
         (to (l2-tx-to tx))
         (amount (l2-tx-amount tx))
         (fee (l2-tx-fee tx))
         (total-cost (+ amount fee))
         (from-account (state-db-get state-db from))
         (to-account (or (state-db-get state-db to)
                         (make-account-state :address to))))

    ;; Debit sender
    (setf (account-state-balance from-account)
          (- (account-state-balance from-account) total-cost))
    (incf (account-state-nonce from-account))

    ;; Credit receiver
    (incf (account-state-balance to-account) amount)

    ;; Update state
    (state-db-put state-db from from-account)
    (state-db-put state-db to to-account)

    (values t nil)))

(defun execute-deposit (state-db tx)
  "Execute a deposit transaction (L1 -> L2)."
  (let* ((to (l2-tx-to tx))
         (amount (l2-tx-amount tx))
         (account (or (state-db-get state-db to)
                      (make-account-state :address to))))
    (incf (account-state-balance account) amount)
    (state-db-put state-db to account)
    (values t nil)))

(defun execute-withdraw (state-db tx)
  "Execute a withdrawal transaction (L2 -> L1)."
  (let* ((from (l2-tx-from tx))
         (amount (l2-tx-amount tx))
         (fee (l2-tx-fee tx))
         (account (state-db-get state-db from)))

    (unless account
      (return-from execute-withdraw (values nil :account-not-found)))

    (let ((total-cost (+ amount fee)))
      (when (< (account-state-balance account) total-cost)
        (return-from execute-withdraw (values nil :insufficient-balance)))

      (decf (account-state-balance account) total-cost)
      (incf (account-state-nonce account))
      (state-db-put state-db from account)
      (values t nil))))

;;; ============================================================================
;;; Batch Production
;;; ============================================================================

(defun process-pending-transactions (rollup &optional (max-txs +batch-target-size+))
  "Process pending transactions from mempool into a batch."
  (unless (rollup-running-p rollup)
    (return-from process-pending-transactions nil))

  (let ((mempool (rollup-state-mempool rollup))
        (builder (rollup-state-batch-builder rollup))
        (state-db (rollup-state-state-db rollup))
        (processed 0))

    ;; Get transactions from mempool
    (loop while (and (< processed max-txs)
                     (not (queue-empty-p (mempool-queue mempool))))
          for tx = (mempool-get-best mempool)
          while tx
          do (multiple-value-bind (valid-p error)
                 (validate-transaction tx state-db)
               (if valid-p
                   (progn
                     (add-transaction builder tx)
                     (incf processed))
                   ;; Remove invalid transactions
                   (mempool-remove mempool (l2-tx-hash tx)))))
    processed))

(defun produce-batch (rollup)
  "Produce a new batch from pending transactions."
  (let* ((builder (rollup-state-batch-builder rollup))
         (state-db (rollup-state-state-db rollup))
         (old-root (get-current-root state-db))
         (transactions (get-pending-transactions builder)))

    (when (null transactions)
      (return-from produce-batch nil))

    ;; Apply all transactions
    (dolist (tx transactions)
      (apply-transaction rollup tx))

    ;; Commit state changes
    (state-db-commit state-db)

    ;; Create batch
    (let* ((new-root (get-current-root state-db))
           (batch (create-batch builder old-root)))
      (setf (l2-batch-new-state-root batch) new-root)

      ;; Track batch
      (let ((tracker (rollup-state-batch-tracker rollup)))
        (tracker-add-batch tracker batch))

      ;; Update stats
      (incf (gethash :batch-count (rollup-state-stats rollup) 0))

      ;; Clear builder for next batch
      (finalize-batch builder batch)

      batch)))

(defun seal-batch (rollup batch)
  "Seal a batch (mark as ready for proving)."
  (let ((tracker (rollup-state-batch-tracker rollup)))
    (tracker-move-batch tracker (l2-batch-id batch) +batch-status-proving+)
    batch))

(defun apply-batch (rollup batch)
  "Apply an entire batch to the rollup state."
  (let ((state-db (rollup-state-state-db rollup))
        (old-root (get-current-root state-db)))

    ;; Verify old root matches
    (unless (= old-root (l2-batch-old-state-root batch))
      (return-from apply-batch (values nil :root-mismatch)))

    ;; Apply all transactions
    (dolist (tx (l2-batch-transactions batch))
      (multiple-value-bind (success error)
          (apply-transaction rollup tx)
        (unless success
          (return-from apply-batch (values nil error)))))

    ;; Commit and verify new root
    (state-db-commit state-db)
    (let ((computed-root (get-current-root state-db)))
      (if (= computed-root (l2-batch-new-state-root batch))
          (values t nil)
          (values nil :invalid-new-root)))))

;;; ============================================================================
;;; Batch Submission
;;; ============================================================================

(defun submit-batch (rollup batch)
  "Prepare a batch for L1 submission."
  (let* ((state-db (rollup-state-state-db rollup))
         (registry (rollup-state-commitment-registry rollup))
         (tracker (rollup-state-batch-tracker rollup))
         (commitment (create-commitment batch state-db))
         (proof (create-commitment-proof commitment state-db batch)))

    ;; Register commitment
    (register-commitment registry commitment)

    ;; Update batch status
    (tracker-move-batch tracker (l2-batch-id batch) +batch-status-submitted+)

    ;; Return submission data
    (list :batch-id (l2-batch-id batch)
          :commitment commitment
          :proof proof
          :calldata (commitment-calldata proof))))

;;; ============================================================================
;;; Finality Tracking
;;; ============================================================================

(defun track-finality (rollup batch-id l1-block)
  "Track finality of a submitted batch."
  (let* ((tracker (rollup-state-batch-tracker rollup))
         (batch (tracker-get-batch tracker batch-id)))
    (when batch
      (when (>= l1-block (+ l1-block +commitment-finality-blocks+))
        (tracker-move-batch tracker batch-id +batch-status-finalized+)
        t))))

(defun is-finalized-p (rollup batch-id)
  "Check if a batch is finalized."
  (let* ((tracker (rollup-state-batch-tracker rollup))
         (batch (tracker-get-batch tracker batch-id)))
    (and batch (eq (l2-batch-status batch) +batch-status-finalized+))))

(defun get-finality-status (rollup batch-id)
  "Get detailed finality status for a batch."
  (let* ((tracker (rollup-state-batch-tracker rollup))
         (batch (tracker-get-batch tracker batch-id))
         (registry (rollup-state-commitment-registry rollup)))
    (when batch
      (let ((commitment (get-commitment-by-batch registry batch-id)))
        (list :batch-id batch-id
              :status (l2-batch-status batch)
              :finalized (is-finalized-p rollup batch-id)
              :commitment-id (when commitment
                               (state-commitment-batch-id commitment))
              :l1-block (when commitment
                          (state-commitment-l1-block commitment)))))))

;;; ============================================================================
;;; Statistics
;;; ============================================================================

(defun rollup-stats (rollup)
  "Get rollup statistics."
  (let ((stats (rollup-state-stats rollup))
        (state-db (rollup-state-state-db rollup))
        (mempool (rollup-state-mempool rollup))
        (tracker (rollup-state-batch-tracker rollup)))
    (list :chain-id (rollup-state-chain-id rollup)
          :status (rollup-state-status rollup)
          :tx-count (gethash :tx-count stats 0)
          :batch-count (gethash :batch-count stats 0)
          :mempool-size (mempool-size mempool)
          :state-root (get-current-root state-db)
          :tracker-stats (tracker-stats tracker)
          :uptime (- (current-timestamp)
                     (gethash :start-time stats (current-timestamp))))))

(defun get-throughput (rollup)
  "Calculate transactions per second."
  (let* ((stats (rollup-state-stats rollup))
         (tx-count (gethash :tx-count stats 0))
         (start-time (gethash :start-time stats (current-timestamp)))
         (elapsed (- (current-timestamp) start-time)))
    (if (> elapsed 0)
        (/ tx-count elapsed)
        0)))

(defun get-pending-count (rollup)
  "Get count of pending transactions."
  (mempool-size (rollup-state-mempool rollup)))

;;; ============================================================================
;;; State Access
;;; ============================================================================

(defun rollup-get-account (rollup address)
  "Get an account from the rollup state."
  (get-account (rollup-state-state-db rollup) address))

(defun rollup-get-balance (rollup address)
  "Get an account balance."
  (get-balance (rollup-state-state-db rollup) address))

(defun rollup-get-nonce (rollup address)
  "Get an account nonce."
  (get-nonce (rollup-state-state-db rollup) address))

(defun rollup-get-root (rollup)
  "Get the current state root."
  (get-current-root (rollup-state-state-db rollup)))

;;; ============================================================================
;;; Snapshot/Restore
;;; ============================================================================

(defun rollup-create-snapshot (rollup name)
  "Create a snapshot of the rollup state."
  (create-snapshot (rollup-state-state-db rollup) name))

(defun rollup-restore-snapshot (rollup name)
  "Restore rollup state from a snapshot."
  (restore-snapshot (rollup-state-state-db rollup) name))
