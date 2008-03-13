;;; -*- Mode: Scheme; scheme48-package: soosy-helpers -*-

;; '((name class-record) ...)
(define *class-descriptors* '())
;; '((generic-id lambda) ...)
(define *generic-functions* '())

(define *generic-function-counter* 0)

(define (name->class name)
  (if (false? name)
      name
      (lookup name *class-descriptors* "unknown class")))

(define (generic-functions-ref id)
  (lookup id *generic-functions*
          "unknown generic function"))

(define (lookup key store message)
  (let ((entry (assq key store)))
    (if (not entry)
        (warn message key)
        (cdr entry))))

(define (false? x) (not x))
