;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

(load "cl-l2.asd")
(handler-case
  (progn
    (asdf:test-system :cl-l2/test)
    (format t "PASS~%"))
  (error (e)
    (format t "FAIL~%")))
(quit)
