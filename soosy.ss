(module soosy scheme
  (require (lib "1.ss"  "srfi")
           (lib "9.ss"  "srfi")
           (rename-in (lib "13.ss" "srfi") (string-hash s:string-hash))
           (lib "14.ss" "srfi")
           (lib "23.ss" "srfi")
           (lib "28.ss" "srfi")
           (lib "69.ss" "srfi"))

  (include "macros.scm")
  (include "xform.scm")
  (include "helpers.scm")
  (include "class.scm")

  (provide define-class
           define-method
           with-instance-variables
           ==>
           usual==>
           make-class make-object
           object-class object-variables object-methods object-method
           object-variable set-object-variable!
           send send-usual
           class? object? object-of-class? base-class? subclass?
           class-name class-superclass class-subclasses class-methods
           class-method-define class-variables))

;;
;; Local Variables:
;; eval: (put 'module 'scheme-indent-function 2)
;; End:
;;