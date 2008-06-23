;;; -*- Mode: Scheme; scheme48-package: soosy-macros -*-

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

