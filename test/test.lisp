;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(defpackage #:cl-l2.test
  (:use #:cl #:cl-l2)
  (:export #:run-tests))

(in-package #:cl-l2.test)

(defun run-tests ()
  (format t "Executing functional test suite for cl-l2...~%")
  (assert (equal (deep-copy-list '(1 (2 3) 4)) '(1 (2 3) 4)))
  (assert (equal (group-by-count '(1 2 3 4 5) 2) '((1 2) (3 4) (5))))
  (format t "All functional tests passed!~%")
  t)
