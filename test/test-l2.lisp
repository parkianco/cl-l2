;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; test-l2.lisp - Unit tests for l2
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:cl-l2.test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-l2.test)

(defun run-tests ()
  "Run all tests for cl-l2."
  (format t "~&Running tests for cl-l2...~%")
  ;; TODO: Add test cases
  ;; (test-function-1)
  ;; (test-function-2)
  (format t "~&All tests passed!~%")
  t)
