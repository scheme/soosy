;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface soosy-macros/interface
  (export (define-class   :syntax)
          (define-generic :syntax)
          (with-instance-variables :syntax)
          ;; (define-method  :syntax)
))

(define-interface soosy-objects/interface
  (export make-class make-object make-method
          method-body method-inherited? set-method-inherited!
          object-class object-variables object-methods
          class? object? object-of-class? base-class? method?
          class-name class-superclass class-methods
          class-variables))

(define-interface soosy-helpers/interface
  (compound-interface
   soosy-objects/interface
   (export (make-getters :syntax)
           make-setters
           *class-descriptors*
           *generic-functions*
           *generic-function-counter*
           false? name->class
           generic-functions-ref)))

(define-interface soosy/interface
  (compound-interface soosy-macros/interface
                      soosy-objects/interface))
