;;; -*- Mode: Scheme; scheme48-package: soosy-macros -*-

(define-syntax define-class
  (syntax-rules ()
    ((define-class class-name superclass (inst-vars ...))
     (define class-name (make-class 'class-name superclass '(inst-vars ...))))))

;; (define-syntax define-class
;;   (lambda (form rename compare)
;;     (if (not (and (= (length form) 4)
;;                   ;; check class name
;;                   (symbol?     (second form))
;;                   ;; check superclass
;;                   (or (symbol? (third form))
;;                       (false?  (third form)))
;;                   ;; check variables
;;                   (proper-list? (fourth form))))
;;         (syntax-error "define-class class-name superclass (var1 var2 ...)")
;;         (let* ((name           (second form))
;;                (superclass     (third form))
;;                (variables      (fourth form))
;;                (%define        (rename 'define))
;;                (%make-class    (rename 'make-class)))
;;           ;; compile-time definition
;;           (make-class name (name->class superclass) variables)
;;           ;; run-time definition
;;           `(,%define ,name (,%make-class ',name ,superclass ',variables))))))

(define-syntax define-method
  (syntax-rules (lambda)
    ((define-method class (operation object arguments ...) body0 body ...)
     (class-method-define class 'operation
                          (lambda (object arguments ...) body0 body ...)))
    ((define-method class operation method)
     (class-method-define class 'operation method))))

;; Warning: invalid expression
;; (#{Name #{Generated let-syntax 1462}}
;;  ((#{Generated set! 1462} (#{Generated syntax-rules 1462} (x) (# #) (# #)))))
;; '((lambda (x) (begin 'syntax-error x)) (object-variable pt 'x))

(define-syntax with-instance-variables
  (syntax-rules ()
    ((_ class instance (instvar0 instvar ...) body0 body ...)
     (let ((instvar0 (object-variable instance 'instvar0))
           (instvar  (object-variable instance 'instvar)) ...)
       (let-syntax ((%set! (syntax-rules () ((%set! old new) (set! old new)))))
         (let-syntax
             ((set!
               (syntax-rules (instvar0 instvar ...)
                 ((set! instvar0 value)
                  (begin (set-object-variable! instance 'instvar0 value)
                         (%set! instvar0 value)))
                 ((set! instvar value)
                  (begin (set-object-variable! instance 'instvar value)
                         (%set! instvar value))) ...
                 ((set! local value)
                  (%set! local value)))))
           body0
           body ...))))))

(define-syntax ==>
  (syntax-rules ()
    ((==> object operation argument ...)
     (send object 'operation argument ...))))

(define-syntax usual==>
  (syntax-rules ()
    ((==> object operation argument ...)
     (send-usual object 'operation argument ...))))
