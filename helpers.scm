;;; -*- Mode: Scheme; scheme48-package: soosy-helpers -*-

;; '((name class-record) ...)
(define *all-classes* (make-hash-table))

(define (name->class name)
  (hash-table-ref/default *all-classes* name #f))

(define (add-class! name class)
  (hash-table-set! *all-classes* name class))

(define (false? x) (eq? x #f))

(define (transform-expression body inst-var? set-expr? ->ref ->set)
  (define (transform e) (transform-expression e inst-var? set-expr? ->ref ->set))
  (let ((expr body))
    (cond
     ((null? expr) expr)
     ((name? expr) (if (inst-var? expr) (->ref expr) expr))
     ((list? expr)
      (cond
       ((set-expr? expr) =>
        (lambda (ivar+value) (->set (car ivar+value)
                                    (cdr ivar+value))))
       (else (cons (transform (car expr))
                   (transform (cdr expr))))))
     (else expr))))

(define (with-instance-variables* expression rename compare)
  (if (< (length expression) 5)
      (syntax-error "(with-instance-variables class instance (ivars ...) code)"))
  (let ((instance (list-ref expression 2))
        (ivars    (list-ref expression 3))
        (code     (cddddr   expression))
        (%ref     (rename 'object-variable))
        (%set!    (rename 'set-object-variable!)))
    (if (null? ivars)
        (syntax-error "not accessing any ivars" expression)
        (transform-expression
         `(,(rename 'begin) ,@code)           ; body
         (lambda (name) (member name ivars))  ; inst-var?
         (lambda (expr) (and (= 3 (length expr))
                             (compare (first expr) (rename 'set!))
                             (cons (second expr)
                                   (third  expr)))) ; set-expr?
         (lambda (ivar) `(,%ref ,instance ',ivar))  ; ->ref
         (lambda (ivar value) `(,%set! ,instance ',ivar ,value)) ; ->set
         ))))