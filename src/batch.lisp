;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; cl-l2/src/batch.lisp - Transaction Batching
;;;; Transaction collection, ordering, and batch management
;;;;
;;;; Provides:
;;;; - L2 transaction structure
;;;; - Priority queue for transaction ordering
;;;; - Mempool management
;;;; - Batch building and lifecycle

(in-package #:cl-l2)

;;; ============================================================================
;;; L2 Transaction
;;; ============================================================================

(defstruct (l2-tx
            (:constructor make-l2-tx
                (&key (type :transfer) from to (amount 0) (fee 0)
                      (nonce 0) signature data))
            (:copier copy-l2-tx))
  "Layer 2 transaction.

   FIELDS:
   - TYPE: Transaction type (:transfer, :deposit, :withdrawal, :contract-call)
   - FROM: Sender address
   - TO: Recipient address
   - AMOUNT: Transfer amount
   - FEE: Transaction fee
   - NONCE: Sender's transaction counter
   - SIGNATURE: Transaction signature (list of components)
   - DATA: Additional transaction data"
  (type :transfer :type keyword)
  (from 0 :type (integer 0))
  (to 0 :type (integer 0))
  (amount 0 :type (integer 0))
  (fee 0 :type (integer 0))
  (nonce 0 :type (integer 0))
  (signature nil :type list)
  (data nil))

(defun l2-tx-hash (tx)
  "Compute hash of transaction."
  (poseidon-hash
   (ecase (l2-tx-type tx)
     (:transfer 0)
     (:deposit 1)
     (:withdrawal 2)
     (:contract-call 3))
   (l2-tx-from tx)
   (l2-tx-to tx)
   (l2-tx-amount tx)
   (l2-tx-fee tx)
   (l2-tx-nonce tx)))

(defun l2-tx-total-cost (tx)
  "Total cost of transaction (amount + fee)."
  (+ (l2-tx-amount tx) (l2-tx-fee tx)))

(defun l2-tx-message (tx)
  "Get the message to be signed for this transaction."
  (poseidon-hash-4
   (l2-tx-from tx)
   (l2-tx-to tx)
   (l2-tx-amount tx)
   (l2-tx-nonce tx)))

(defun l2-tx-valid-structure-p (tx)
  "Check if transaction has valid structure."
  (and (valid-address-p (l2-tx-from tx))
       (valid-address-p (l2-tx-to tx))
       (valid-amount-p (l2-tx-amount tx))
       (>= (l2-tx-fee tx) 0)
       (valid-nonce-p (l2-tx-nonce tx))))

;;; ============================================================================
;;; L2 Batch
;;; ============================================================================

(defstruct (l2-batch
            (:constructor make-l2-batch
                (&key (id 0) transactions (old-state-root 0)
                      (new-state-root 0) (timestamp 0)
                      (status +batch-status-pending+)
                      compressed-data))
            (:copier copy-l2-batch))
  "Batch of L2 transactions.

   FIELDS:
   - ID: Batch identifier
   - TRANSACTIONS: List of transactions in batch
   - OLD-STATE-ROOT: State root before batch
   - NEW-STATE-ROOT: State root after batch
   - TIMESTAMP: Creation timestamp
   - STATUS: Batch lifecycle status
   - COMPRESSED-DATA: Compressed transaction data for L1 posting"
  (id 0 :type (integer 0))
  (transactions nil :type list)
  (old-state-root 0 :type (integer 0))
  (new-state-root 0 :type (integer 0))
  (timestamp 0 :type (integer 0))
  (status +batch-status-pending+ :type keyword)
  (compressed-data nil))

(defun l2-batch-size (batch)
  "Number of transactions in batch."
  (length (l2-batch-transactions batch)))

(defun l2-batch-total-fees (batch)
  "Sum of all fees in batch."
  (reduce #'+ (l2-batch-transactions batch)
          :key #'l2-tx-fee
          :initial-value 0))

;;; ============================================================================
;;; Priority Transaction Queue
;;; ============================================================================

(defstruct (priority-tx-queue
            (:constructor %make-priority-tx-queue)
            (:copier nil))
  "Priority queue for transaction ordering.

   FIELDS:
   - HEAP: Binary heap of (priority . tx) pairs
   - SIZE: Current queue size
   - CAPACITY: Maximum capacity
   - ORDERING: Ordering strategy"
  (heap (make-array 1024 :adjustable t :fill-pointer 0) :type vector)
  (size 0 :type (integer 0))
  (capacity +batch-max-size+ :type (integer 1))
  (ordering :fee :type keyword))

(defun make-priority-tx-queue (&key (capacity +batch-max-size+) (ordering :fee))
  "Create a new priority transaction queue."
  (%make-priority-tx-queue :capacity capacity :ordering ordering))

(defun tx-priority (tx ordering)
  "Compute priority value for transaction."
  (ecase ordering
    (:fee (l2-tx-fee tx))
    (:nonce (- (l2-tx-nonce tx)))
    (:fifo 0)))

(defun enqueue-tx (queue tx)
  "Add transaction to queue.
   Returns T if successful, NIL if full."
  (when (>= (priority-tx-queue-size queue)
            (priority-tx-queue-capacity queue))
    (return-from enqueue-tx nil))
  (let* ((priority (tx-priority tx (priority-tx-queue-ordering queue)))
         (heap (priority-tx-queue-heap queue)))
    (vector-push-extend (cons priority tx) heap)
    (incf (priority-tx-queue-size queue))
    (heap-bubble-up heap (1- (length heap)))
    t))

(defun dequeue-tx (queue)
  "Remove and return highest priority transaction."
  (when (zerop (priority-tx-queue-size queue))
    (return-from dequeue-tx nil))
  (let* ((heap (priority-tx-queue-heap queue))
         (result (cdr (aref heap 0))))
    (setf (aref heap 0) (aref heap (1- (length heap))))
    (decf (fill-pointer heap))
    (decf (priority-tx-queue-size queue))
    (when (> (length heap) 0)
      (heap-bubble-down heap 0))
    result))

(defun peek-tx (queue)
  "Return highest priority transaction without removing."
  (when (> (priority-tx-queue-size queue) 0)
    (cdr (aref (priority-tx-queue-heap queue) 0))))

(defun heap-bubble-up (heap index)
  "Restore heap property by bubbling up."
  (loop while (> index 0)
        for parent = (floor (1- index) 2)
        while (> (car (aref heap index)) (car (aref heap parent)))
        do (rotatef (aref heap index) (aref heap parent))
           (setf index parent)))

(defun heap-bubble-down (heap index)
  "Restore heap property by bubbling down."
  (let ((size (length heap)))
    (loop
      (let ((left (1+ (* 2 index)))
            (right (+ 2 (* 2 index)))
            (largest index))
        (when (and (< left size)
                   (> (car (aref heap left)) (car (aref heap largest))))
          (setf largest left))
        (when (and (< right size)
                   (> (car (aref heap right)) (car (aref heap largest))))
          (setf largest right))
        (when (= largest index)
          (return))
        (rotatef (aref heap index) (aref heap largest))
        (setf index largest)))))

(defun queue-empty-p (queue)
  "Check if queue is empty."
  (zerop (priority-tx-queue-size queue)))

(defun queue-full-p (queue)
  "Check if queue is at capacity."
  (>= (priority-tx-queue-size queue)
      (priority-tx-queue-capacity queue)))

(defun queue-clear (queue)
  "Clear all transactions from queue."
  (setf (fill-pointer (priority-tx-queue-heap queue)) 0)
  (setf (priority-tx-queue-size queue) 0))

;;; ============================================================================
;;; Mempool
;;; ============================================================================

(defstruct (mempool
            (:constructor %make-mempool)
            (:copier nil))
  "Transaction mempool.

   FIELDS:
   - TRANSACTIONS: Hash of tx-hash -> tx
   - BY-SENDER: Hash of sender -> list of tx hashes
   - BY-NONCE: Hash of (sender . nonce) -> tx-hash
   - SIZE: Current size
   - MAX-SIZE: Maximum capacity
   - EVICTION-POLICY: Policy for eviction when full"
  (transactions (make-hash-table :test 'equal) :type hash-table)
  (by-sender (make-hash-table :test 'eql) :type hash-table)
  (by-nonce (make-hash-table :test 'equal) :type hash-table)
  (size 0 :type (integer 0))
  (max-size +max-mempool-size+ :type (integer 1))
  (eviction-policy :lowest-fee :type keyword)
  (stats-added 0 :type (integer 0))
  (stats-removed 0 :type (integer 0))
  (stats-evicted 0 :type (integer 0)))

(defun make-mempool (&key (max-size +max-mempool-size+)
                          (eviction-policy :lowest-fee))
  "Create a new mempool."
  (%make-mempool :max-size max-size :eviction-policy eviction-policy))

(defun mempool-add (mempool tx)
  "Add transaction to mempool.
   Returns (values success-p error-message)."
  (let ((tx-hash (l2-tx-hash tx))
        (sender (l2-tx-from tx))
        (nonce (l2-tx-nonce tx)))
    ;; Check duplicates
    (when (gethash tx-hash (mempool-transactions mempool))
      (return-from mempool-add (values nil "Already in mempool")))
    ;; Check nonce collision
    (let ((existing (gethash (cons sender nonce) (mempool-by-nonce mempool))))
      (when existing
        (let ((existing-tx (gethash existing (mempool-transactions mempool))))
          (if (and existing-tx (> (l2-tx-fee tx) (l2-tx-fee existing-tx)))
              (mempool-remove mempool existing-tx)
              (return-from mempool-add (values nil "Nonce already used"))))))
    ;; Evict if necessary
    (when (>= (mempool-size mempool) (mempool-max-size mempool))
      (mempool-evict-one mempool))
    ;; Add transaction
    (setf (gethash tx-hash (mempool-transactions mempool)) tx)
    (push tx-hash (gethash sender (mempool-by-sender mempool)))
    (setf (gethash (cons sender nonce) (mempool-by-nonce mempool)) tx-hash)
    (incf (mempool-size mempool))
    (incf (mempool-stats-added mempool))
    (values t nil)))

(defun mempool-remove (mempool tx)
  "Remove transaction from mempool."
  (let ((tx-hash (l2-tx-hash tx))
        (sender (l2-tx-from tx))
        (nonce (l2-tx-nonce tx)))
    (when (gethash tx-hash (mempool-transactions mempool))
      (remhash tx-hash (mempool-transactions mempool))
      (setf (gethash sender (mempool-by-sender mempool))
            (remove tx-hash (gethash sender (mempool-by-sender mempool))))
      (remhash (cons sender nonce) (mempool-by-nonce mempool))
      (decf (mempool-size mempool))
      (incf (mempool-stats-removed mempool))
      t)))

(defun mempool-get-best (mempool count)
  "Get COUNT highest priority transactions."
  (let ((all-txs nil))
    (maphash (lambda (hash tx)
               (declare (ignore hash))
               (push tx all-txs))
             (mempool-transactions mempool))
    (setf all-txs (sort all-txs #'> :key #'l2-tx-fee))
    (subseq all-txs 0 (min count (length all-txs)))))

(defun mempool-evict-one (mempool)
  "Evict one transaction according to policy."
  (let ((to-evict nil))
    (ecase (mempool-eviction-policy mempool)
      (:lowest-fee
       (maphash (lambda (hash tx)
                  (declare (ignore hash))
                  (when (or (null to-evict)
                            (< (l2-tx-fee tx) (l2-tx-fee to-evict)))
                    (setf to-evict tx)))
                (mempool-transactions mempool)))
      (:oldest
       (maphash (lambda (hash tx)
                  (declare (ignore hash))
                  (unless to-evict (setf to-evict tx)))
                (mempool-transactions mempool))))
    (when to-evict
      (mempool-remove mempool to-evict)
      (incf (mempool-stats-evicted mempool)))
    to-evict))

;; Note: mempool-size accessor is automatically generated by defstruct

(defun mempool-contains-p (mempool tx-hash)
  "Check if mempool contains transaction."
  (not (null (gethash tx-hash (mempool-transactions mempool)))))

(defun mempool-get-by-sender (mempool sender)
  "Get all transactions from sender."
  (let ((hashes (gethash sender (mempool-by-sender mempool)))
        (result nil))
    (dolist (hash hashes)
      (let ((tx (gethash hash (mempool-transactions mempool))))
        (when tx (push tx result))))
    (sort result #'< :key #'l2-tx-nonce)))

(defun mempool-clear (mempool)
  "Clear all transactions."
  (clrhash (mempool-transactions mempool))
  (clrhash (mempool-by-sender mempool))
  (clrhash (mempool-by-nonce mempool))
  (setf (mempool-size mempool) 0))

(defun mempool-stats (mempool)
  "Get mempool statistics."
  (list :size (mempool-size mempool)
        :max-size (mempool-max-size mempool)
        :added (mempool-stats-added mempool)
        :removed (mempool-stats-removed mempool)
        :evicted (mempool-stats-evicted mempool)))

;;; ============================================================================
;;; Transaction Ordering
;;; ============================================================================

(defun order-transactions (txs &key (strategy :optimal))
  "Order transactions for batch inclusion."
  (ecase strategy
    (:fee (order-by-fee txs))
    (:nonce (order-by-nonce txs))
    (:optimal (order-optimal txs))))

(defun order-by-fee (txs)
  "Order by fee (highest first)."
  (sort (copy-list txs) #'> :key #'l2-tx-fee))

(defun order-by-nonce (txs)
  "Order by nonce within each sender."
  (let ((by-sender (make-hash-table :test 'eql)))
    (dolist (tx txs)
      (push tx (gethash (l2-tx-from tx) by-sender)))
    (maphash (lambda (sender sender-txs)
               (setf (gethash sender by-sender)
                     (sort sender-txs #'< :key #'l2-tx-nonce)))
             by-sender)
    (let ((result nil))
      (maphash (lambda (sender txs)
                 (declare (ignore sender))
                 (dolist (tx txs) (push tx result)))
               by-sender)
      result)))

(defun order-optimal (txs)
  "Order optimally: nonce order with fee tie-breaking."
  (let ((by-sender (make-hash-table :test 'eql)))
    (dolist (tx txs)
      (push tx (gethash (l2-tx-from tx) by-sender)))
    (maphash (lambda (sender sender-txs)
               (setf (gethash sender by-sender)
                     (sort sender-txs #'< :key #'l2-tx-nonce)))
             by-sender)
    (let ((heads nil)
          (result nil))
      (maphash (lambda (sender sender-txs)
                 (when sender-txs
                   (push (cons sender (first sender-txs)) heads)))
               by-sender)
      (loop while heads
            do (setf heads (sort heads #'> :key (lambda (h) (l2-tx-fee (cdr h)))))
               (let* ((best (pop heads))
                      (sender (car best))
                      (tx (cdr best))
                      (remaining (cdr (gethash sender by-sender))))
                 (push tx result)
                 (setf (gethash sender by-sender) remaining)
                 (when remaining
                   (push (cons sender (first remaining)) heads))))
      (nreverse result))))

(defun detect-conflicts (txs)
  "Detect conflicts in transaction set."
  (let ((conflicts nil)
        (by-nonce (make-hash-table :test 'equal)))
    (dolist (tx txs)
      (let ((key (cons (l2-tx-from tx) (l2-tx-nonce tx))))
        (when (gethash key by-nonce)
          (push (list tx (gethash key by-nonce) :nonce-conflict) conflicts))
        (setf (gethash key by-nonce) tx)))
    conflicts))

(defun resolve-dependencies (txs)
  "Resolve dependencies and return executable order."
  (order-by-nonce txs))

;;; ============================================================================
;;; Transaction Validation
;;; ============================================================================

(defun validate-transaction (tx state-db)
  "Validate transaction against state.
   Returns (values valid-p errors)."
  (let ((errors nil))
    (unless (l2-tx-valid-structure-p tx)
      (push :invalid-structure errors))
    (let ((sender (get-account state-db (l2-tx-from tx))))
      (when (account-state-empty-p sender)
        (push :sender-not-found errors))
      (unless (>= (account-state-balance sender) (l2-tx-total-cost tx))
        (push :insufficient-balance errors))
      (unless (= (l2-tx-nonce tx) (account-state-nonce sender))
        (push :invalid-nonce errors)))
    (values (null errors) errors)))

;;; ============================================================================
;;; Batch Building
;;; ============================================================================

(defstruct (batch-builder
            (:constructor %make-batch-builder)
            (:copier nil))
  "Builder for creating batches."
  (next-id 1 :type (integer 1))
  (transactions nil :type list)
  (size 0 :type (integer 0))
  (target-size +batch-target-size+ :type (integer 1))
  (total-fees 0 :type (integer 0))
  (start-time 0 :type (integer 0))
  (state-db nil)
  (old-root 0 :type (integer 0)))

(defun make-batch-builder (state-db &key (target-size +batch-target-size+))
  "Create a new batch builder."
  (%make-batch-builder
   :target-size target-size
   :state-db state-db
   :old-root (get-current-root state-db)
   :start-time (get-universal-time)))

(defun add-transaction (builder tx)
  "Add transaction to batch being built.
   Returns (values success-p reason)."
  (when (>= (batch-builder-size builder) (batch-builder-target-size builder))
    (return-from add-transaction (values nil :batch-full)))
  ;; Check conflicts
  (dolist (existing (batch-builder-transactions builder))
    (when (and (= (l2-tx-from tx) (l2-tx-from existing))
               (= (l2-tx-nonce tx) (l2-tx-nonce existing)))
      (return-from add-transaction (values nil :conflict))))
  (push tx (batch-builder-transactions builder))
  (incf (batch-builder-size builder))
  (incf (batch-builder-total-fees builder) (l2-tx-fee tx))
  (values t nil))

(defun remove-transaction (builder tx)
  "Remove transaction from batch being built."
  (when (member tx (batch-builder-transactions builder) :test #'eq)
    (setf (batch-builder-transactions builder)
          (remove tx (batch-builder-transactions builder) :test #'eq))
    (decf (batch-builder-size builder))
    (decf (batch-builder-total-fees builder) (l2-tx-fee tx))
    t))

(defun get-pending-transactions (builder)
  "Get transactions in current batch."
  (copy-list (batch-builder-transactions builder)))

(defun create-batch (builder)
  "Create batch from builder state."
  (let* ((txs (order-optimal (batch-builder-transactions builder)))
         (batch-id (batch-builder-next-id builder))
         (state-db (batch-builder-state-db builder)))
    (incf (batch-builder-next-id builder))
    ;; Create snapshot
    (create-snapshot state-db (format nil "pre-batch-~D" batch-id))
    ;; Execute and compute new root
    (let ((temp-db (state-db-clone state-db))
          (successful nil))
      (dolist (tx txs)
        (multiple-value-bind (success new-root err)
            (apply-transaction temp-db tx)
          (declare (ignore new-root err))
          (when success (push tx successful))))
      (make-l2-batch
       :id batch-id
       :transactions (nreverse successful)
       :old-state-root (batch-builder-old-root builder)
       :new-state-root (get-current-root temp-db)
       :timestamp (get-universal-time)
       :status +batch-status-pending+))))

(defun finalize-batch (batch)
  "Finalize batch transactions."
  (setf (l2-batch-transactions batch)
        (order-optimal (l2-batch-transactions batch)))
  batch)

;;; ============================================================================
;;; Batch Lifecycle
;;; ============================================================================

(defstruct (batch-tracker
            (:constructor make-batch-tracker)
            (:copier nil))
  "Tracks batches through lifecycle."
  (pending nil :type list)
  (proving nil :type list)
  (proved nil :type list)
  (submitted nil :type list)
  (confirmed nil :type list)
  (finalized nil :type list)
  (rejected nil :type list)
  (by-id (make-hash-table :test 'eql) :type hash-table))

(defun tracker-add-batch (tracker batch)
  "Add batch to tracker."
  (push batch (batch-tracker-pending tracker))
  (setf (gethash (l2-batch-id batch) (batch-tracker-by-id tracker)) batch))

(defun tracker-move-batch (tracker batch new-status)
  "Move batch to new status."
  (let ((old-status (l2-batch-status batch)))
    ;; Remove from old list
    (ecase old-status
      (:pending (setf (batch-tracker-pending tracker)
                      (remove batch (batch-tracker-pending tracker))))
      (:proving (setf (batch-tracker-proving tracker)
                      (remove batch (batch-tracker-proving tracker))))
      (:proved (setf (batch-tracker-proved tracker)
                     (remove batch (batch-tracker-proved tracker))))
      (:submitted (setf (batch-tracker-submitted tracker)
                        (remove batch (batch-tracker-submitted tracker))))
      (:confirmed (setf (batch-tracker-confirmed tracker)
                        (remove batch (batch-tracker-confirmed tracker)))))
    ;; Update status
    (setf (l2-batch-status batch) new-status)
    ;; Add to new list
    (ecase new-status
      (:pending (push batch (batch-tracker-pending tracker)))
      (:proving (push batch (batch-tracker-proving tracker)))
      (:proved (push batch (batch-tracker-proved tracker)))
      (:submitted (push batch (batch-tracker-submitted tracker)))
      (:confirmed (push batch (batch-tracker-confirmed tracker)))
      (:finalized (push batch (batch-tracker-finalized tracker)))
      (:rejected (push batch (batch-tracker-rejected tracker))))))

(defun tracker-get-batch (tracker batch-id)
  "Get batch by ID."
  (gethash batch-id (batch-tracker-by-id tracker)))

(defun tracker-stats (tracker)
  "Get tracker statistics."
  (list :pending (length (batch-tracker-pending tracker))
        :proving (length (batch-tracker-proving tracker))
        :proved (length (batch-tracker-proved tracker))
        :submitted (length (batch-tracker-submitted tracker))
        :confirmed (length (batch-tracker-confirmed tracker))
        :finalized (length (batch-tracker-finalized tracker))
        :rejected (length (batch-tracker-rejected tracker))
        :total (hash-table-count (batch-tracker-by-id tracker))))

;;; ============================================================================
;;; Batch Serialization
;;; ============================================================================

(defun serialize-tx (tx stream)
  "Serialize transaction to stream."
  (write-byte (ecase (l2-tx-type tx)
                (:transfer 0) (:deposit 1)
                (:withdrawal 2) (:contract-call 3))
              stream)
  (write-varint (l2-tx-from tx) stream)
  (write-varint (l2-tx-to tx) stream)
  (write-varint (l2-tx-amount tx) stream)
  (write-varint (l2-tx-fee tx) stream)
  (write-varint (l2-tx-nonce tx) stream))

(defun deserialize-tx (stream)
  "Deserialize transaction from stream."
  (let ((type-byte (read-byte stream)))
    (make-l2-tx
     :type (ecase type-byte
             (0 :transfer) (1 :deposit)
             (2 :withdrawal) (3 :contract-call))
     :from (read-varint stream)
     :to (read-varint stream)
     :amount (read-varint stream)
     :fee (read-varint stream)
     :nonce (read-varint stream))))

(defun serialize-batch (batch stream)
  "Serialize batch to stream."
  (write-varint (l2-batch-id batch) stream)
  (write-varint (l2-batch-old-state-root batch) stream)
  (write-varint (l2-batch-new-state-root batch) stream)
  (write-varint (l2-batch-timestamp batch) stream)
  (write-varint (length (l2-batch-transactions batch)) stream)
  (dolist (tx (l2-batch-transactions batch))
    (serialize-tx tx stream)))

(defun deserialize-batch (stream)
  "Deserialize batch from stream."
  (let ((id (read-varint stream))
        (old-root (read-varint stream))
        (new-root (read-varint stream))
        (timestamp (read-varint stream))
        (tx-count (read-varint stream))
        (txs nil))
    (dotimes (i tx-count)
      (push (deserialize-tx stream) txs))
    (make-l2-batch
     :id id
     :old-state-root old-root
     :new-state-root new-root
     :timestamp timestamp
     :transactions (nreverse txs))))

(defun compress-batch (batch)
  "Compress batch for L1 posting."
  ;; Stub: serialize batch to bytes (no compression yet)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)))
    ;; For now, return placeholder - actual implementation would:
    ;; 1. Serialize batch structure to bytes
    ;; 2. Apply compression (e.g., zlib)
    (declare (ignore batch))
    buffer))

(defun decompress-batch (data)
  "Decompress batch from L1 data."
  (with-input-from-string (s (map 'string #'code-char data))
    (deserialize-batch s)))
