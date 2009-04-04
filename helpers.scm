;;; -*- Mode: Scheme; scheme48-package: soosy-helpers -*-

;; '((name class-record) ...)
(define *all-classes* (make-hash-table))

(define (name->class name)
  (hash-table-ref/default *all-classes* name #f))

(define (add-class! name class)
  (hash-table-set! *all-classes* name class))

(define (false? x) (eq? x #f))

(define (generate-ivar-bindings ->name ->body ivars)
  (map (lambda (ivar)
         `(,(->name ivar)
           ,(->body ivar)))
       ivars))

;;;
;;; This procedure provides the expansion for the WITH-INSTANCE-VARIABLES macro
;;;
;;; The expansion contains two parts:
;;;
;;; (1) the LET part generates local bindings for each of the ivars using OBJECT-VARIABLE
;;; (2) the LET-SYNTAX part generates 'syntax-rules' macros for SET! and SET-IVAR!
;;;     * SET! on an ivar will expand into a warning, otherwise it falls back to the normal SET!
;;;     * SET-IVAR! on an ivar will expand into SET-OBJECT-VARIABLE, otherwise it will expand into a warning.
;;;
(define (with-instance-variables* expression rename compare)
  (if (< (length expression) 5)
      (syntax-error "(with-instance-variables class instance (ivars ...) code)"))
  (let ((instance      (list-ref expression 2))
        (ivars         (list-ref expression 3))
        (code          (drop expression 4))
        (%let          (rename 'let))
        (%let-syntax   (rename 'let-syntax))
        (%ref          (rename 'object-variable))
        (%set-ivar!    (rename 'set-object-variable!))
        (%syntax-rules (rename 'syntax-rules))
        (%warn         (rename 'warn)))
    `(,%let
      (,@(generate-ivar-bindings
          (lambda (ivar) ivar)
          (lambda (ivar) `(,%ref ,instance ',ivar))
          ivars))
      (,%let-syntax
       ((set-ivar!
         (,%syntax-rules
          ,ivars
          ,@(generate-ivar-bindings
             (lambda (ivar) `(set-ivar! ,ivar value))
             (lambda (ivar) `(,%set-ivar! ,instance ',ivar value))
             ivars)
          ((set-ivar! var value) (,%warn "this is not an ivar" 'var))))
        (set!
         (,%syntax-rules
          ,ivars
          ,@(generate-ivar-bindings
             (lambda (ivar) `(set! ,ivar value))
             (lambda (ivar)
               `(,%warn "this is an ivar, use SET-IVAR! instead" ',ivar))
             ivars)
          ((set! var value) (set! var value)))))
       ,@code))))
