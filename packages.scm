;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure soosy-helpers soosy-helpers/interface
  (open scheme define-record-type* signals srfi-1 srfi-69)
  (for-syntax (open scheme signals srfi-1))
  (files class helpers))

(define-structure soosy-macros soosy-macros/interface
  (open scheme define-record-type* signals soosy-helpers srfi-1)
  (for-syntax (open scheme signals soosy-helpers srfi-1))
  (files macros))

(define-structure soosy soosy/interface
  (open scheme soosy-macros soosy-helpers))

(define-structure define-record-type*
    (export (define-record-type* :syntax)
            define-record-type
            define-record-discloser)
  (open scheme define-record-types)
  (for-syntax (open scheme define-record-type*-expander))
  (begin (define-syntax define-record-type*
           expand-define-record-type*
           (BEGIN DEFINE DEFINE-RECORD-TYPE))))

(define-structure define-record-type*-expander
    (export expand-define-record-type*)
  (open scheme destructuring fluids signals receiving)
  (files defrectype))

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