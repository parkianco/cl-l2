;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-l2 Package Definitions
;;;; Pure Common Lisp Layer 2 Rollup Primitives
;;;;
;;;; This package provides standalone L2 scaling primitives:
;;;; - State management with sparse Merkle trees
;;;; - Transaction batching and ordering
;;;; - State commitments and proofs
;;;; - Rollup coordination logic

(in-package #:cl-user)

;;; ============================================================================
;;; Main L2 Package
;;; ============================================================================

(defpackage #:cl-l2
  (:use #:cl)
  (:nicknames #:l2)
  (:documentation
   "Pure Common Lisp Layer 2 Rollup Primitives.

    This library provides the essential building blocks for L2 rollup systems:

    STATE MANAGEMENT
    - Sparse Merkle tree for account state
    - Efficient batch updates with dirty path tracking
    - Snapshot/restore for rollback capability
    - State proofs for verification

    BATCH PROCESSING
    - Transaction collection and validation
    - Priority queue ordering (by fee, nonce, time)
    - Mempool management with eviction policies
    - Conflict detection and resolution

    COMMITMENTS
    - State root computation
    - Merkle proof generation and verification
    - Commitment tracking and finalization
    - Historical root access

    ROLLUP COORDINATION
    - Batch lifecycle management
    - Sequencer state tracking
    - L1 submission preparation
    - Finality tracking

    ZERO EXTERNAL DEPENDENCIES - uses only standard CL and SBCL threading.")

  ;; ============================================================================
  ;; State Management Exports
  ;; ============================================================================
  (:export
   ;; State Database
   #:state-db
   #:make-state-db
   #:state-db-p
   #:state-db-clone

   ;; Account State
   #:account-state
   #:make-account-state
   #:account-state-p
   #:account-state-address
   #:account-state-balance
   #:account-state-nonce
   #:account-state-pubkey-hash
   #:account-state-storage-root
   #:account-state-empty-p
   #:copy-account-state

   ;; State Operations
   #:state-db-get
   #:state-db-put
   #:state-db-delete
   #:state-db-batch-update
   #:state-db-commit

   ;; State Queries
   #:get-account
   #:get-balance
   #:get-nonce
   #:account-exists-p
   #:get-current-root
   #:get-historical-root
   #:verify-root-transition

   ;; Snapshots
   #:create-snapshot
   #:restore-snapshot
   #:list-snapshots
   #:delete-snapshot

   ;; State Diff
   #:state-diff
   #:make-state-diff
   #:state-diff-old-root
   #:state-diff-new-root
   #:state-diff-changes
   #:compute-state-diff
   #:apply-state-diff

   ;; State Integrity
   #:verify-state-integrity
   #:state-db-stats)

  ;; ============================================================================
  ;; Sparse Merkle Tree Exports
  ;; ============================================================================
  (:export
   ;; Tree Structure
   #:sparse-merkle-tree
   #:make-sparse-merkle-tree
   #:sparse-merkle-tree-p
   #:sparse-merkle-tree-depth
   #:copy-sparse-merkle-tree

   ;; Tree Operations
   #:smt-get
   #:smt-set
   #:smt-root
   #:smt-prove
   #:smt-verify
   #:smt-batch-update

   ;; Merkle Proofs
   #:merkle-proof
   #:make-merkle-proof
   #:merkle-proof-p
   #:merkle-proof-path
   #:merkle-proof-siblings
   #:merkle-proof-root
   #:merkle-proof-leaf-index
   #:merkle-proof-leaf-value
   #:merkle-proof-verify

   ;; Constants
   #:+default-tree-depth+
   #:+empty-leaf-hash+)

  ;; ============================================================================
  ;; Batch Processing Exports
  ;; ============================================================================
  (:export
   ;; L2 Transaction
   #:l2-tx
   #:make-l2-tx
   #:l2-tx-p
   #:l2-tx-type
   #:l2-tx-from
   #:l2-tx-to
   #:l2-tx-amount
   #:l2-tx-fee
   #:l2-tx-nonce
   #:l2-tx-signature
   #:l2-tx-data
   #:l2-tx-hash
   #:l2-tx-total-cost
   #:l2-tx-message

   ;; Batch Structure
   #:l2-batch
   #:make-l2-batch
   #:l2-batch-p
   #:l2-batch-id
   #:l2-batch-transactions
   #:l2-batch-old-state-root
   #:l2-batch-new-state-root
   #:l2-batch-timestamp
   #:l2-batch-status
   #:l2-batch-size
   #:l2-batch-total-fees
   #:l2-batch-compressed-data

   ;; Priority Queue
   #:priority-tx-queue
   #:make-priority-tx-queue
   #:enqueue-tx
   #:dequeue-tx
   #:peek-tx
   #:queue-empty-p
   #:queue-full-p
   #:queue-clear

   ;; Mempool
   #:mempool
   #:make-mempool
   #:mempool-add
   #:mempool-remove
   #:mempool-get-best
   #:mempool-size
   #:mempool-contains-p
   #:mempool-get-by-sender
   #:mempool-clear
   #:mempool-stats

   ;; Transaction Ordering
   #:order-transactions
   #:order-by-fee
   #:order-by-nonce
   #:detect-conflicts
   #:resolve-dependencies

   ;; Batch Building
   #:batch-builder
   #:make-batch-builder
   #:add-transaction
   #:remove-transaction
   #:get-pending-transactions
   #:create-batch
   #:finalize-batch

   ;; Transaction Validation
   #:validate-transaction

   ;; Batch Lifecycle
   #:batch-tracker
   #:make-batch-tracker
   #:tracker-add-batch
   #:tracker-move-batch
   #:tracker-get-batch
   #:tracker-stats

   ;; Batch Status Constants
   #:+batch-status-pending+
   #:+batch-status-proving+
   #:+batch-status-proved+
   #:+batch-status-submitted+
   #:+batch-status-confirmed+
   #:+batch-status-finalized+
   #:+batch-status-rejected+

   ;; Configuration
   #:+batch-target-size+
   #:+batch-max-size+
   #:+batch-timeout+
   #:+max-mempool-size+)

  ;; ============================================================================
  ;; Commitment Exports
  ;; ============================================================================
  (:export
   ;; Commitment Structure
   #:state-commitment
   #:make-state-commitment
   #:state-commitment-p
   #:state-commitment-batch-id
   #:state-commitment-state-root
   #:state-commitment-timestamp
   #:state-commitment-proof
   #:state-commitment-l1-block
   #:state-commitment-l1-tx-hash
   #:state-commitment-finalized-p

   ;; Commitment Operations
   #:create-commitment
   #:verify-commitment
   #:finalize-commitment

   ;; Commitment Registry
   #:commitment-registry
   #:make-commitment-registry
   #:register-commitment
   #:get-commitment
   #:get-commitments-in-range
   #:get-latest-commitment
   #:get-pending-commitments

   ;; Proof Generation
   #:generate-state-proof
   #:generate-account-proof
   #:generate-multi-proof
   #:verify-account-proof

   ;; Commitment Constants
   #:+commitment-finality-blocks+)

  ;; ============================================================================
  ;; Rollup Coordination Exports
  ;; ============================================================================
  (:export
   ;; Rollup State
   #:rollup-state
   #:make-rollup-state
   #:rollup-state-p
   #:rollup-state-chain-id
   #:rollup-state-current-batch
   #:rollup-state-state-db
   #:rollup-state-mempool
   #:rollup-state-batch-tracker
   #:rollup-state-commitment-registry
   #:rollup-state-status

   ;; Rollup Lifecycle
   #:initialize-rollup
   #:start-rollup
   #:stop-rollup
   #:rollup-status

   ;; Transaction Processing
   #:submit-transaction
   #:process-pending-transactions
   #:apply-transaction
   #:apply-batch

   ;; Batch Production
   #:produce-batch
   #:seal-batch
   #:submit-batch

   ;; Finality
   #:track-finality
   #:is-finalized-p
   #:get-finality-status

   ;; Statistics
   #:rollup-stats
   #:get-throughput
   #:get-pending-count

   ;; Rollup Status Constants
   #:+rollup-status-initializing+
   #:+rollup-status-running+
   #:+rollup-status-paused+
   #:+rollup-status-stopped+)

  ;; ============================================================================
  ;; Utility Exports
  ;; ============================================================================
  (:export
   ;; Hashing
   #:poseidon-hash
   #:poseidon-hash-2
   #:poseidon-hash-4
   #:sha256
   #:keccak256

   ;; Serialization
   #:serialize-tx
   #:deserialize-tx
   #:serialize-batch
   #:deserialize-batch
   #:compress-batch
   #:decompress-batch

   ;; Threading
   #:with-lock
   #:make-lock

   ;; Encoding
   #:bytes-to-integer
   #:integer-to-bytes
   #:hex-encode
   #:hex-decode))

;;; ============================================================================
;;; Test Package
;;; ============================================================================

(defpackage #:cl-l2.test
  (:use #:cl #:cl-l2)
  (:export #:run-tests))
