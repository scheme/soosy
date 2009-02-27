;;; -*- Mode: Scheme; scheme48-package: soosy-macros -*-

(define-syntax define-class
  (syntax-rules ()
    ((define-class class-name superclass (inst-vars ...))
     (define class-name (make-class 'class-name superclass '(inst-vars ...))))))

(define-syntax define-method
  (syntax-rules ()
    ((define-method class (operation object arguments ...) body0 body ...)
     (class-method-define class 'operation
                          (lambda (object arguments ...) body0 body ...)))
    ((define-method class operation method)
     (class-method-define class 'operation method))))

(define-syntax ==>
  (syntax-rules ()
    ((==> object operation argument ...)
     (send object 'operation argument ...))))

(define-syntax usual==>
  (syntax-rules ()
    ((==> object operation argument ...)
     (send-usual object 'operation argument ...))))

(define-syntax with-instance-variables with-instance-variables*)