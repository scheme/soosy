;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure soosy-helpers soosy-helpers/interface
  (open scheme define-record-type* signals srfi-1)
  (files class helpers))

(define-structure soosy-macros soosy-macros/interface
  (open scheme define-record-type* signals soosy-helpers srfi-1)
  (for-syntax (open scheme signals soosy-helpers srfi-1))
  (files macros))

(define-structure soosy soosy/interface
  (open scheme soosy-macros soosy-helpers))

(define-structure define-record-type*
    (export (define-record-type* :syntax)
            define-record-type
            define-record-discloser)
  (open scheme define-record-types)
  (for-syntax (open scheme define-record-type*-expander))
  (begin (define-syntax define-record-type*
           expand-define-record-type*
           (BEGIN DEFINE DEFINE-RECORD-TYPE))))

(define-structure define-record-type*-expander
    (export expand-define-record-type*)
  (open scheme destructuring fluids signals receiving)
  (files defrectype))