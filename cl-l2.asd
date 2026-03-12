;;;; cl-l2.asd - Layer 2 Rollup Primitives for Common Lisp
;;;; Pure Common Lisp implementation of L2 scaling primitives
;;;;
;;;; Zero external dependencies - uses only standard CL and SBCL extensions

(asdf:defsystem #:cl-l2
  :name "cl-l2"
  :version "0.1.0"
  :author "Parkian Company LLC"
  :license "MIT"
  :description "Pure Common Lisp Layer 2 rollup primitives: state management, batch processing, commitments, and rollup logic"
  :homepage "https://github.com/parkianco/cl-l2"
  :bug-tracker "https://github.com/parkianco/cl-l2/issues"

  :depends-on ()  ; Zero external dependencies

  :components
  ((:file "package")
   (:module "src"
    :serial t
    :depends-on ("package")
    :components
    ((:file "util")
     (:file "state")
     (:file "batch")
     (:file "commitment")
     (:file "rollup"))))

  :in-order-to ((test-op (test-op #:cl-l2/test))))

(asdf:defsystem #:cl-l2/test
  :depends-on (#:cl-l2)
  :components
  ((:module "test"
    :components
    ((:file "test-l2"))))
  :perform (test-op (op c)
             (let ((result (uiop:symbol-call :cl-l2.test :run-tests)))
               (unless result
                 (error "Tests failed")))))
