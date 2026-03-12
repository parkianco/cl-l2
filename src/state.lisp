;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-l2/src/state.lisp - L2 State Management
;;;; Sparse Merkle tree and account state management
;;;;
;;;; Provides:
;;;; - Sparse Merkle tree for efficient state storage
;;;; - Account state with balance, nonce, storage
;;;; - State database with snapshots and rollback
;;;; - Merkle proof generation and verification

(in-package #:cl-l2)

;;; ============================================================================
;;; Account State
;;; ============================================================================

(defstruct (account-state
            (:constructor make-account-state
                (&key (address 0) (balance 0) (nonce 0)
                      (pubkey-hash 0) (storage-root 0)))
            (:copier copy-account-state))
  "State of a single account in the rollup.

   FIELDS:
   - ADDRESS: Account identifier
   - BALANCE: Token balance
   - NONCE: Transaction counter
   - PUBKEY-HASH: Hash of public key for signature verification
   - STORAGE-ROOT: Root of account's storage tree (for contracts)"
  (address 0 :type (integer 0))
  (balance 0 :type (integer 0))
  (nonce 0 :type (integer 0))
  (pubkey-hash 0 :type (integer 0))
  (storage-root 0 :type (integer 0)))

(defun account-state-empty-p (account)
  "Check if account is empty (zero balance, zero nonce)."
  (and (zerop (account-state-balance account))
       (zerop (account-state-nonce account))))

(defun account-state-equal (a b)
  "Check if two account states are equal."
  (and (= (account-state-address a) (account-state-address b))
       (= (account-state-balance a) (account-state-balance b))
       (= (account-state-nonce a) (account-state-nonce b))
       (= (account-state-pubkey-hash a) (account-state-pubkey-hash b))
       (= (account-state-storage-root a) (account-state-storage-root b))))

;;; ============================================================================
;;; Merkle Proof
;;; ============================================================================

(defstruct (merkle-proof
            (:constructor make-merkle-proof
                (&key path siblings root leaf-index leaf-value))
            (:copier copy-merkle-proof))
  "Merkle inclusion proof for a leaf in the tree.

   FIELDS:
   - PATH: Bit vector indicating left/right at each level
   - SIBLINGS: List of sibling hashes along the path
   - ROOT: Expected root hash
   - LEAF-INDEX: Index of the leaf being proved
   - LEAF-VALUE: Value at the leaf"
  (path nil :type list)
  (siblings nil :type list)
  (root 0 :type (integer 0))
  (leaf-index 0 :type (integer 0))
  (leaf-value 0 :type (integer 0)))

(defun merkle-proof-verify (proof)
  "Verify a Merkle proof.
   Returns T if proof is valid, NIL otherwise."
  (let ((hash (merkle-proof-leaf-value proof))
        (index (merkle-proof-leaf-index proof)))
    (loop for sibling in (merkle-proof-siblings proof)
          for level from 0
          do (if (logbitp level index)
                 (setf hash (poseidon-hash-2 sibling hash))
                 (setf hash (poseidon-hash-2 hash sibling))))
    (= hash (merkle-proof-root proof))))

;;; ============================================================================
;;; Sparse Merkle Tree
;;; ============================================================================

(defstruct (sparse-merkle-tree
            (:constructor %make-sparse-merkle-tree)
            (:copier nil))
  "Sparse Merkle tree for efficient state storage.

   FIELDS:
   - DEPTH: Tree depth (number of levels)
   - LEAVES: Hash table of index -> leaf value
   - NODES: Hash table of (level . index) -> node hash
   - EMPTY-HASHES: Precomputed hashes for empty subtrees at each level
   - ROOT-CACHE: Cached root hash (invalidated on updates)"
  (depth +default-tree-depth+ :type (integer 1))
  (leaves (make-hash-table :test 'eql) :type hash-table)
  (nodes (make-hash-table :test 'equal) :type hash-table)
  (empty-hashes nil :type (or null vector))
  (root-cache nil :type (or null integer)))

(defun make-sparse-merkle-tree (&key (depth +default-tree-depth+))
  "Create a new sparse Merkle tree."
  (let ((tree (%make-sparse-merkle-tree :depth depth)))
    ;; Precompute empty subtree hashes
    (let ((empties (make-array (1+ depth) :initial-element +empty-leaf-hash+)))
      (loop for level from 1 to depth
            do (setf (aref empties level)
                     (poseidon-hash-2 (aref empties (1- level))
                                      (aref empties (1- level)))))
      (setf (sparse-merkle-tree-empty-hashes tree) empties))
    tree))

(defun copy-sparse-merkle-tree (tree)
  "Create a deep copy of a sparse Merkle tree."
  (let ((new-tree (%make-sparse-merkle-tree
                   :depth (sparse-merkle-tree-depth tree)
                   :leaves (copy-hash-table (sparse-merkle-tree-leaves tree))
                   :nodes (copy-hash-table (sparse-merkle-tree-nodes tree))
                   :empty-hashes (copy-seq (sparse-merkle-tree-empty-hashes tree))
                   :root-cache (sparse-merkle-tree-root-cache tree))))
    new-tree))

(defun smt-get (tree index)
  "Get the leaf value at INDEX."
  (gethash index (sparse-merkle-tree-leaves tree) +empty-leaf-hash+))

(defun smt-set (tree index value)
  "Set the leaf value at INDEX to VALUE.
   Returns the new root hash."
  ;; Update leaf
  (if (zerop value)
      (remhash index (sparse-merkle-tree-leaves tree))
      (setf (gethash index (sparse-merkle-tree-leaves tree)) value))
  ;; Invalidate cache and recompute path
  (setf (sparse-merkle-tree-root-cache tree) nil)
  (smt-update-path tree index)
  (smt-root tree))

(defun smt-update-path (tree index)
  "Update the path from leaf to root after a change at INDEX."
  (let ((depth (sparse-merkle-tree-depth tree))
        (nodes (sparse-merkle-tree-nodes tree))
        (empty-hashes (sparse-merkle-tree-empty-hashes tree)))
    ;; Get leaf hash
    (let ((hash (gethash index (sparse-merkle-tree-leaves tree) +empty-leaf-hash+))
          (idx index))
      ;; Update each level
      (loop for level from 0 below depth
            do (let* ((sibling-idx (logxor idx 1))
                      (sibling-hash (or (gethash (cons level sibling-idx) nodes)
                                        (aref empty-hashes level)))
                      (parent-idx (ash idx -1))
                      (left (if (evenp idx) hash sibling-hash))
                      (right (if (evenp idx) sibling-hash hash))
                      (parent-hash (poseidon-hash-2 left right)))
                 (setf (gethash (cons (1+ level) parent-idx) nodes) parent-hash)
                 (setf hash parent-hash)
                 (setf idx parent-idx))))))

(defun smt-root (tree)
  "Get the root hash of the tree."
  (or (sparse-merkle-tree-root-cache tree)
      (let ((depth (sparse-merkle-tree-depth tree)))
        (setf (sparse-merkle-tree-root-cache tree)
              (or (gethash (cons depth 0) (sparse-merkle-tree-nodes tree))
                  (aref (sparse-merkle-tree-empty-hashes tree) depth))))))

(defun smt-prove (tree index)
  "Generate a Merkle proof for the leaf at INDEX."
  (let ((depth (sparse-merkle-tree-depth tree))
        (nodes (sparse-merkle-tree-nodes tree))
        (empty-hashes (sparse-merkle-tree-empty-hashes tree))
        (siblings nil)
        (idx index))
    ;; Collect siblings along the path
    (loop for level from 0 below depth
          do (let ((sibling-idx (logxor idx 1)))
               (push (or (gethash (cons level sibling-idx) nodes)
                         (aref empty-hashes level))
                     siblings))
             (setf idx (ash idx -1)))
    (make-merkle-proof
     :path (loop for i from 0 below depth collect (logbitp i index))
     :siblings (nreverse siblings)
     :root (smt-root tree)
     :leaf-index index
     :leaf-value (smt-get tree index))))

(defun smt-verify (tree proof)
  "Verify a Merkle proof against this tree's root."
  (and (= (merkle-proof-root proof) (smt-root tree))
       (merkle-proof-verify proof)))

(defun smt-batch-update (tree updates)
  "Apply multiple updates atomically.
   UPDATES is a list of (index . value) pairs.
   Returns the new root hash."
  (setf (sparse-merkle-tree-root-cache tree) nil)
  (dolist (update updates)
    (let ((index (car update))
          (value (cdr update)))
      (if (zerop value)
          (remhash index (sparse-merkle-tree-leaves tree))
          (setf (gethash index (sparse-merkle-tree-leaves tree)) value))))
  ;; Update all affected paths
  (dolist (update updates)
    (smt-update-path tree (car update)))
  (smt-root tree))

;;; ============================================================================
;;; State Database
;;; ============================================================================

(defconstant +state-history-depth+ 256
  "Number of historical state roots to retain.")

(defstruct (state-db
            (:constructor %make-state-db)
            (:copier nil))
  "Database for rollup state management.

   FIELDS:
   - TREE: Sparse Merkle tree for account states
   - ACCOUNTS: Hash table for fast account lookup
   - ROOT-HISTORY: Ring buffer of historical roots
   - SNAPSHOTS: Named snapshots for rollback
   - PENDING-UPDATES: Uncommitted changes
   - LOCK: Mutex for thread-safe access"
  (tree nil :type (or null sparse-merkle-tree))
  (accounts (make-hash-table :test 'eql) :type hash-table)
  (root-history (make-array +state-history-depth+) :type vector)
  (root-history-index 0 :type (integer 0))
  (snapshots (make-hash-table :test 'equal) :type hash-table)
  (pending-updates nil :type list)
  (dirty-paths (make-hash-table :test 'eql) :type hash-table)
  (last-batch-id 0 :type (integer 0))
  (creation-time 0 :type (integer 0))
  (lock nil))

(defun make-state-db (&key (depth +default-tree-depth+))
  "Create a new state database."
  (let ((db (%make-state-db
             :tree (make-sparse-merkle-tree :depth depth)
             :creation-time (get-universal-time)
             :lock (make-lock "state-db"))))
    (setf (aref (state-db-root-history db) 0)
          (smt-root (state-db-tree db)))
    db))

(defun state-db-clone (db)
  "Create a deep copy of the state database."
  (%make-state-db
   :tree (copy-sparse-merkle-tree (state-db-tree db))
   :accounts (copy-hash-table (state-db-accounts db))
   :root-history (copy-seq (state-db-root-history db))
   :root-history-index (state-db-root-history-index db)
   :snapshots (copy-hash-table (state-db-snapshots db))
   :pending-updates (copy-list (state-db-pending-updates db))
   :dirty-paths (copy-hash-table (state-db-dirty-paths db))
   :last-batch-id (state-db-last-batch-id db)
   :creation-time (state-db-creation-time db)
   :lock nil))

;;; ============================================================================
;;; State Database Operations
;;; ============================================================================

(defun state-db-get (db address)
  "Get the account state for ADDRESS.
   Returns NIL if account doesn't exist."
  (gethash address (state-db-accounts db)))

(defun state-db-put (db address account)
  "Store ACCOUNT state at ADDRESS."
  (setf (gethash address (state-db-accounts db)) account)
  (setf (gethash address (state-db-dirty-paths db)) t)
  (push (cons address account) (state-db-pending-updates db))
  account)

(defun state-db-delete (db address)
  "Delete account at ADDRESS."
  (let ((had-account (gethash address (state-db-accounts db))))
    (remhash address (state-db-accounts db))
    (setf (gethash address (state-db-dirty-paths db)) t)
    (push (cons address nil) (state-db-pending-updates db))
    had-account))

(defun state-db-batch-update (db updates)
  "Apply multiple updates atomically.
   UPDATES is a list of (address . account) pairs."
  (dolist (update updates)
    (let ((address (car update))
          (account (cdr update)))
      (if account
          (setf (gethash address (state-db-accounts db)) account)
          (remhash address (state-db-accounts db)))
      (setf (gethash address (state-db-dirty-paths db)) t)))
  (state-db-commit db))

(defun encode-account-to-leaf (account)
  "Encode account state to a field element for tree storage."
  (poseidon-hash-4
   (account-state-balance account)
   (account-state-nonce account)
   (account-state-pubkey-hash account)
   (account-state-storage-root account)))

(defun hash-account-state (account)
  "Hash an account state to a single field element.
   Used for Merkle proof leaf values."
  (encode-account-to-leaf account))

(defun state-db-commit (db)
  "Commit pending updates to the state tree.
   Returns the new state root."
  (let ((tree (state-db-tree db))
        (updates nil))
    ;; Collect dirty updates
    (maphash (lambda (address dirty-p)
               (declare (ignore dirty-p))
               (let ((account (gethash address (state-db-accounts db))))
                 (push (cons address
                             (if account
                                 (encode-account-to-leaf account)
                                 0))
                       updates)))
             (state-db-dirty-paths db))
    ;; Apply to tree
    (smt-batch-update tree updates)
    ;; Clear dirty tracking
    (clrhash (state-db-dirty-paths db))
    (setf (state-db-pending-updates db) nil)
    ;; Record in history
    (let ((idx (mod (incf (state-db-root-history-index db))
                    +state-history-depth+)))
      (setf (aref (state-db-root-history db) idx) (smt-root tree)))
    (smt-root tree)))

;;; ============================================================================
;;; State Queries
;;; ============================================================================

(defun get-account (db address)
  "Get account at ADDRESS, creating empty account if needed."
  (or (state-db-get db address)
      (make-account-state :address address)))

(defun get-balance (db address)
  "Get balance of account at ADDRESS."
  (account-state-balance (get-account db address)))

(defun get-nonce (db address)
  "Get nonce of account at ADDRESS."
  (account-state-nonce (get-account db address)))

(defun account-exists-p (db address)
  "Check if an account exists at ADDRESS."
  (let ((account (state-db-get db address)))
    (and account (not (account-state-empty-p account)))))

(defun get-current-root (db)
  "Get the current state root."
  (smt-root (state-db-tree db)))

(defun get-historical-root (db batch-id)
  "Get the state root after BATCH-ID was processed."
  (aref (state-db-root-history db)
        (mod batch-id +state-history-depth+)))

(defun verify-root-transition (db old-root new-root transitions)
  "Verify that applying TRANSITIONS to OLD-ROOT produces NEW-ROOT."
  (let ((temp-db (state-db-clone db)))
    (unless (= (get-current-root temp-db) old-root)
      (return-from verify-root-transition nil))
    (dolist (transition transitions)
      (state-db-put temp-db (car transition) (cdr transition)))
    (state-db-commit temp-db)
    (= (get-current-root temp-db) new-root)))

;;; ============================================================================
;;; Snapshot Management
;;; ============================================================================

(defun create-snapshot (db name)
  "Create a named snapshot of current state."
  (let ((snapshot (list :root (get-current-root db)
                        :accounts (copy-hash-table (state-db-accounts db))
                        :timestamp (get-universal-time)
                        :batch-id (state-db-last-batch-id db))))
    (setf (gethash name (state-db-snapshots db)) snapshot)
    name))

(defun restore-snapshot (db name)
  "Restore state from a named snapshot.
   Returns T if successful, NIL if not found."
  (let ((snapshot (gethash name (state-db-snapshots db))))
    (when snapshot
      (clrhash (state-db-accounts db))
      (maphash (lambda (k v)
                 (setf (gethash k (state-db-accounts db)) v))
               (getf snapshot :accounts))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (setf (gethash k (state-db-dirty-paths db)) t))
               (state-db-accounts db))
      (state-db-commit db)
      t)))

(defun list-snapshots (db)
  "List all available snapshots."
  (let ((result nil))
    (maphash (lambda (name snapshot)
               (push (list :name name
                           :root (getf snapshot :root)
                           :timestamp (getf snapshot :timestamp)
                           :batch-id (getf snapshot :batch-id))
                     result))
             (state-db-snapshots db))
    (sort result #'> :key (lambda (s) (getf s :timestamp)))))

(defun delete-snapshot (db name)
  "Delete a named snapshot."
  (remhash name (state-db-snapshots db)))

;;; ============================================================================
;;; State Diff
;;; ============================================================================

(defstruct (state-diff
            (:constructor make-state-diff
                (&key old-root new-root changes timestamp))
            (:copier copy-state-diff))
  "Represents a diff between two state roots."
  (old-root 0 :type (integer 0))
  (new-root 0 :type (integer 0))
  (changes nil :type list)
  (timestamp 0 :type (integer 0)))

(defun compute-state-diff (db1 db2)
  "Compute the diff between two state databases."
  (let ((changes nil)
        (all-addrs (make-hash-table :test 'eql)))
    (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k all-addrs) t))
             (state-db-accounts db1))
    (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k all-addrs) t))
             (state-db-accounts db2))
    (maphash (lambda (addr present)
               (declare (ignore present))
               (let ((acc1 (state-db-get db1 addr))
                     (acc2 (state-db-get db2 addr)))
                 (unless (and acc1 acc2 (account-state-equal acc1 acc2))
                   (push (list addr acc1 acc2) changes))))
             all-addrs)
    (make-state-diff
     :old-root (get-current-root db1)
     :new-root (get-current-root db2)
     :changes changes
     :timestamp (get-universal-time))))

(defun apply-state-diff (db diff)
  "Apply a state diff to a database."
  (unless (= (get-current-root db) (state-diff-old-root diff))
    (error "State root mismatch"))
  (dolist (change (state-diff-changes diff))
    (let ((addr (first change))
          (new-state (third change)))
      (if new-state
          (state-db-put db addr new-state)
          (state-db-delete db addr))))
  (state-db-commit db))

;;; ============================================================================
;;; Proof Generation
;;; ============================================================================

(defun generate-account-proof (db address)
  "Generate a Merkle proof for an account."
  (when (state-db-pending-updates db)
    (state-db-commit db))
  (smt-prove (state-db-tree db) address))

(defun generate-multi-proof (db addresses)
  "Generate proofs for multiple accounts."
  (mapcar (lambda (addr)
            (cons addr (generate-account-proof db addr)))
          addresses))

(defun verify-account-proof (db proof address expected-value)
  "Verify a Merkle proof for an account."
  (and (= (merkle-proof-leaf-index proof) address)
       (= (merkle-proof-leaf-value proof) expected-value)
       (= (merkle-proof-root proof) (smt-root (state-db-tree db)))
       (merkle-proof-verify proof)))

;;; ============================================================================
;;; State Statistics
;;; ============================================================================

(defun verify-state-integrity (db)
  "Verify integrity of state database.
   Returns (values valid-p errors)."
  (let ((errors nil))
    (maphash (lambda (addr account)
               (let ((tree-val (smt-get (state-db-tree db) addr))
                     (expected (encode-account-to-leaf account)))
                 (unless (= tree-val expected)
                   (push (list :mismatch addr tree-val expected) errors))))
             (state-db-accounts db))
    (values (null errors) errors)))

(defun state-db-stats (db)
  "Get statistics about state database."
  (list :total-accounts (hash-table-count (state-db-accounts db))
        :tree-depth (sparse-merkle-tree-depth (state-db-tree db))
        :tree-leaves (hash-table-count (sparse-merkle-tree-leaves (state-db-tree db)))
        :current-root (get-current-root db)
        :snapshots (hash-table-count (state-db-snapshots db))
        :pending-updates (length (state-db-pending-updates db))
        :creation-time (state-db-creation-time db)))
