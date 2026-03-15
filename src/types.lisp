;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-l2)

;;; Core types for cl-l2
(deftype cl-l2-id () '(unsigned-byte 64))
(deftype cl-l2-status () '(member :ready :active :error :shutdown))
