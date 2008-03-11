;;; -*- Mode: Scheme; scheme48-package: edwin:object -*-

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
                 (body             (third  form))
                 (%CAR             (rename 'car))
                 (%COUNTER         (rename '*generic-function-counter*))
                 (%IF              (rename 'if))
                 (%LAMBDA          (rename 'lambda))
                 (%LET*            (rename 'let*))
                 (%METHODS-REF     (rename 'methods-ref))
                 (%OBJECT-METHODS  (rename 'object-methods))
                 (generic-function
                  `(,%LAMBDA ,params
                             (,%LET* ((methods (,%OBJECT-METHODS (,%car ,params)))
                                      (handler (,%METHODS-REF methods ,%COUNTER)))
                                     (,%if handler
                                           (handler ,@params)
                                           body)))))
            (set! *generic-functions*
                  (alist-cons generic-function *generic-function-counter*
                              *generic-functions*))
            generic-function)))))

(define-syntax define-class
  (lambda (form rename compare)
    (if (not (and (= (length form) 4)
                  ;; check class name
                  (symbol?      (second form))
                  ;; check superclass
                  (or (class? (third form))
                      (not    (third form)))
                  ;; check variables
                  (proper-list? (fourth form))))
        (syntax-error "define-class class-name superclass (var1 var2 ...)")
        (let ((name           (second form))
              (superclass     (if (null? (third form)) #f (third form)))
              (variables      (fourth form))
              (%define        (rename 'define))
              (%define-syntax (rename 'define-syntax))
              (%lambda        (rename 'lambda))
              (%let           (rename 'let))
              (%make-class    (rename 'make-class)))
          ;; compile-time definition
          (make-class name superclass variables)
          ;; run-time definition
          `(,%define ,name (,%make-class ',name ,superclass ',variables))))))

(define-syntax make-getters
  (lambda (form rename compare)
    (if (not (= 2 (length form)))
        (syntax-error "make-getters variables")
        (let* ((variables     (second form))
               (var-count     (length variables))
               (%syntax-rules (rename 'syntax-rules))
               (%vector-ref   (rename 'vector-ref)))
          (let loop ((index   0)
                     (vars    variables)
                     (getters '(,%syntax-rules '())))
            (if (> index var-count)
                getters
                (loop (+ 1 index)
                      (cdr vars)
                      (append getters
                              `((,(car vars))
                                (,%vector-ref ,variables ,index))))))))))

(define-syntax make-setters
  (lambda (form rename compare)
    (if (not (= 2 (length form)))
        (syntax-error "make-setters variables")
        (let* ((variables     (second form))
               (var-count     (length variables))
               (%set!         (rename 'set!))
               (%syntax-rules (rename 'syntax-rules))
               (%vector-set!  (rename 'vector-set!)))
          (let loop ((index 0)
                     (vars variables)
                     (setters '(,%syntax-rules ,variables)))
            (if (> index var-count)
                (append setters
                        '((set! variable value)
                          (%set! variable value)))
                (loop (+ 1 index)
                      (cdr vars)
                      (append setters
                              `((set! ,(first vars) value)
                                (vector-set! ,variables ,index value))))))))))
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

