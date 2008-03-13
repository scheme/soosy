;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface soosy-macros/interface
  (export (define-class   :syntax)
          (define-generic :syntax)
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
   (export *class-descriptors*
           *generic-functions*
           *generic-function-counter*
           false?
           methods-ref name->class)))

(define-interface soosy/interface
  (compound-interface soosy-macros/interface
                      soosy-objects/interface))
