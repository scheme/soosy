;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure soosy-helpers soosy-helpers/interface
  (open scheme srfi-1 srfi-9 srfi-23 srfi-69)
  (for-syntax (open scheme srfi-1 srfi-23))
  (files class helpers))

(define-structure soosy-macros soosy-macros/interface
  (open scheme soosy-helpers srfi-1 srfi-9)
  (for-syntax (open scheme soosy-helpers srfi-1))
  (files macros xform))

(define-structure soosy soosy/interface
  (open scheme soosy-macros soosy-helpers soosy-disclosers))

;;; SRFI 69: Basic hash tables
;;;
;;; Implement this using the tables module

(define-interface srfi-69-interface
  (export make-hash-table hash-table? alist->hash-table
          ;; hash-table-equivalence-function, hash-table-hash-function
          hash-table-ref hash-table-ref/default
          hash-table-set! hash-table-delete! hash-table-exists?
          ;; hash-table-update! hash-table-update!/default
          hash-table-size hash-table-keys hash-table-values
          hash-table-walk ;; hash-table-fold
          hash-table->alist hash-table-copy ;; hash-table-merge!
          hash string-hash ;; string-ci-hash hash-by-identity
          ))

(define-structure srfi-69 srfi-69-interface
  (open scheme
        (subset signals (error))
        define-opt
        (modify tables (rename (default-hash-function hash))))
  (files srfi-69))

(define-structure define-opt
    (export (define* :syntax))
  (open scheme srfi-1 let-opt)
  (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))
  (files define-opt))