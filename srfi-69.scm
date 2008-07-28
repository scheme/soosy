;;; -*- Mode: Scheme; scheme48-package: srfi-69 -*-
;;;
;;; SRFI 69: Basic hash tables
;;;

(define* (make-hash-table (equal? equal?) (hash hash))
  ((make-table-maker equal? hash)))

(define (hash-table? t) (table? t))

(define* (alist->hash-table alist (equal? equal?) (hash hash))
  (let ((table (make-hash-table equal? hash)))
    (for-each (lambda (x) (hash-table-set! table (car x) (cdr x)))
              alist)
    table))

(define (hash-table-ref table key . thunk)
  (let ((value (table-ref table key)))
    (if (eq? value #f)
        (if (null? thunk) (error "key not found") ((car thunk)))
        value)))

(define (hash-table-ref/default table key default)
  (hash-table-ref table key (lambda () default)))

(define (hash-table-set! table key value)
  (table-set! table key value))

(define (hash-table-delete! table key)
  (table-set! table key #f))

(define (hash-table-exists? table key)
  (hash-table-ref table key (lambda () #f)))

(define (hash-table-size table)
  (let ((size 0))
    (table-walk
     (lambda (k v) (set! size (+ size 1)))
     table)
    size))

(define (hash-table-keys table)
  (let ((keys '()))
    (table-walk
     (lambda (k v) (set! keys (cons k keys)))
     table)
    keys))

(define (hash-table-values table)
  (let ((values '()))
    (table-walk
     (lambda (k v) (set! values (cons v values)))
     table)
    values))

(define (hash-table-walk table proc)
  (table-walk proc table))

(define (hash-table->alist table)
  (let ((alist '()))
    (table-walk
     (lambda (k v) (set! alist (cons (list k v) alist)))
     table)
    alist))

(define (hash-table-copy table)
  (let ((copy (make-hash-table)))
    (table-walk
     (lambda (k v) (hash-table-set! copy k v))
     table)
    copy))

; according to srfi-69 "hash function is acceptable for equal?"

(define (hash obj . rest)
  (if (not (null? rest))
      (let ((bound (car rest)))
	(if (and (integer? bound) (> bound 0))
	    (let ((h (hash obj)))
	      (if (> h bound)
		  (remainder h bound)
		  h))
	    (error "invalid argument"
		   '(not (> integer 0))
		   `(while calling ,hash)
		   `(received ,bound))))
      (cond
       ((pair? obj) (+ (hash (car obj))
		       (* 3 (hash (cdr obj)))))
       ((vector? obj) (hash (vector->list obj))) ; lazy mofo
       ((string? obj) (string-hash obj))
       (else (default-hash-function obj)))))
