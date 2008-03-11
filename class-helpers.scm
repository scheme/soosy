
;; '((name class-record) ...)
(define *class-descriptors* '())
;; '((generic-id lambda) ...)
(define *generic-functions* '())

(define *generic-function-counter* 0)

(define (name->class name)
  (lookup name *class-descriptors* "unknown class"))

(define (methods-ref id)
  (lookup id *generic-functions* "unknown generic function"))

(define (lookup key store message)
  (let ((entry (assq key store)))
    (if (not entry)
        (error message key)
        (cdr entry))))


(define (false? x) (not x))
