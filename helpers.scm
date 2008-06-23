;;; -*- Mode: Scheme; scheme48-package: soosy-helpers -*-

;; '((name class-record) ...)
(define *all-classes* (make-hash-table))

(define (name->class name)
  (hash-table-ref/default *all-classes* name #f))

(define (add-class! name class)
  (hash-table-set! *all-classes* name class))

(define (false? x) (eq? x #f))


