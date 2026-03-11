# cl-l2

Pure Common Lisp Layer 2 rollup primitives with **zero external dependencies**.

## Features

- **State management**: Rollup state tree operations
- **Batch processing**: Transaction batching and compression
- **Commitments**: State root and batch commitments
- **Fraud proofs**: Optimistic rollup dispute mechanism
- **Validity proofs**: ZK rollup proof verification
- **Pure Common Lisp**: No CFFI, no external libraries

## Installation

```lisp
(asdf:load-system :cl-l2)
```

## Quick Start

```lisp
(use-package :cl-l2)

;; Create rollup state
(let ((rollup (make-rollup-state)))
  ;; Process transactions
  (rollup-process-tx rollup
                     :from "0x..."
                     :to "0x..."
                     :amount 1000)
  ;; Create batch
  (let ((batch (rollup-create-batch rollup)))
    ;; Submit to L1
    (rollup-submit-batch rollup batch)))
```

## API Reference

### State Management

- `(make-rollup-state)` - Create new rollup state
- `(rollup-get-balance state address)` - Get account balance
- `(rollup-get-nonce state address)` - Get account nonce

### Transaction Processing

- `(rollup-process-tx state &key from to amount)` - Process transaction
- `(rollup-create-batch state)` - Create batch from pending txs
- `(rollup-submit-batch state batch)` - Submit batch to L1

### Proofs

- `(rollup-generate-fraud-proof state batch tx-index)` - Generate fraud proof
- `(rollup-verify-fraud-proof proof)` - Verify fraud proof

## Testing

```lisp
(asdf:test-system :cl-l2)
```

## License

BSD-3-Clause

Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
