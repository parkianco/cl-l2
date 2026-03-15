;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-l2)

(define-condition cl-l2-error (error)
  ((message :initarg :message :reader cl-l2-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-l2 error: ~A" (cl-l2-error-message condition)))))
