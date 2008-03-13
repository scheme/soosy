;;; -*- Mode: Scheme; scheme48-package: soosy-macros -*-

(define-syntax define-generic
  (syntax-rules ()
    ((DEFINE-GENERIC (name param))
     (define name (generic (param) (lambda () (error "no default body")))))
    ((DEFINE-GENERIC (name params ...))
     (define name (generic (params ...) (lambda () (error "no default body")))))
    ((DEFINE-GENERIC (name params ...) default-body ...)
     (define name (generic (params ...) default-body ...)))))

(define-syntax generic
  (lambda (form rename compare)
    (if (> (length form) 3)
        (syntax-error "generic params body")
        (begin
          (set! *generic-function-counter* (+ 1 *generic-function-counter*))
          (let* ((params           (second form))
                 (default-body     (third  form))
                 (self             (first params))
                 (%CAR             (rename 'car))
                 (%COUNTER         (rename '*generic-function-counter*))
                 (%IF              (rename 'if))
                 (%LAMBDA          (rename 'lambda))
                 (%LET*            (rename 'let*))
                 (%LOOKUP          (rename 'generic-functions-ref))
                 (%OBJECT-METHODS  (rename 'object-methods))
                 (generic-function
                  `(,%LAMBDA ,params
                             (,%LET* ((methods (,%OBJECT-METHODS ,self))
                                      (handler (,%LOOKUP methods ,%COUNTER)))
                                     (,%if handler
                                           (handler ,@params)
                                           default-body)))))
            (set! *generic-functions*
                  (alist-cons generic-function *generic-function-counter*
                              *generic-functions*))
            generic-function)))))

(define-syntax define-class
  (lambda (form rename compare)
    (if (not (and (= (length form) 4)
                  ;; check class name
                  (symbol?     (second form))
                  ;; check superclass
                  (or (symbol? (third form))
                      (false?  (third form)))
                  ;; check variables
                  (proper-list? (fourth form))))
        (syntax-error "define-class class-name superclass (var1 var2 ...)")
        (let* ((name           (second form))
               (superclass     (third form))
               (variables      (fourth form))
               (%define        (rename 'define))
               (%make-class    (rename 'make-class)))
          ;; compile-time definition
          (make-class name (name->class superclass) variables)
          ;; run-time definition
          `(,%define ,name (,%make-class ',name ,superclass ',variables))))))

#|

(define-syntax with-instance-variables
  (syntax-rules ()
    ((with-instance-variables <instance> (instvar ...) body ...)
     (let-syntax ((set! (make-setters <instance> (instvar ...))))
       (let (make-getters <instance> (instvar ...))
         body ...)))))

|#

(define-syntax with-instance-variables
  (lambda (form rename compare)
    (if (< (length form) 4)
        (syntax-error "with-instance-variables <instance> (instvar ...) body ...")
        (let* ((instance      (second form))
               (variables     (third form))
               (body          (drop form 3))
               (%let          (rename 'let))
               (%let-syntax   (rename 'let-syntax))
               (%make-setters (rename 'make-setters))
               (%rename       (rename 'rename)))
          `(,%let-syntax
            ((set! (,%make-setters ',instance ',variables ,%rename)))
            (,%let (make-getters ,instance ',variables)
                   ,@body))))))

#|
(define-syntax define-method
  (lambda (form rename compare)
    (if (not (= 3 (length form)))
        (syntax-error "define-method class-name generic-function lambda")
        (let* ((%class-method-define
               (rename 'class-method-define))
               (class-name (second form))
               (generic    (third  form))
               (body       (fourth form))
               (class      (name->class class-name))
               (inst-vars  (class-variables class))
               (getters    (make-getters inst-vars))
               (setters    (make-setters inst-vars)))))))
|#

