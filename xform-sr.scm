;;; define the following two macros appropriately:
;;;
;;; (instance-variable-reference <class> <instance> <variable>)
;;;   should expand to an expression for the value of <variable> in
;;;   <instance>, an instance of <class>.  <class> will be the literal
;;;   name of a class; <instance> will be a simple expressions that are
;;;   ok to evaluate multiple times.  <variable> will be the literal
;;;   name of a variable (not a quotation or anything).
;;;
;;; (instance-variable-assignment <class> <instance> <variable>)
;;;   should expand to an expression whose value is a unary procedure
;;;   accepting a new value for <variable> in <instance>, an instance
;;;   of <class>, that should store the value in <instance>.
;;;
;;; here are two bogus definitions for the case when all the
;;; information we have is dynamic.

(define-syntax instance-variable-reference
  (syntax-rules ()
    ((instance-variable-reference class instance variable)
     (*instance-variable-reference 'class instance 'variable))))

(define-syntax instance-variable-assignment
  (syntax-rules ()
    ((instance-variable-assignment class instance variable)
     (*instance-variable-assignment 'class instance 'variable))))

(define (*instance-variable-reference class instance variable)
  (object-variable instance variable))

(define (*instance-variable-assignment class instance variable)
  (lambda (value)
    (set-object-variable! instance variable value)))

;;; with the above macros defined, you may use with-instance-variables.

(define-syntax with-instance-variables
  (syntax-rules ()
    ((with-instance-variables class-name instance-expression (variable ...)
         body0 body1+ ...)
     (let ((instance instance-expression))
       (with-variable-substitutions
           ((variable
             (instance-variable-reference class-name instance variable)
             (instance-variable-assignment class-name instance variable))
            ...)
         (let () body0 body1+ ...))))))

(define-syntax with-variable-substitutions
  (syntax-rules ()
    ((with-variable-substitutions substitutions expression)
     (substitute-variables (with-variable-substitutions/finish)
                           substitutions
                           expression))))

(define-syntax with-variable-substitutions/finish
  (syntax-rules ()
    ((with-variable-substitutions/finish expression)
     expression)))

;;; rudimentary syntactic cps facility.

(define-syntax syntax-continue
  (syntax-rules ()
    ((syntax-continue (macro environment ...) . arguments)
     (macro environment ... . arguments))))

;;;; the real hair

;;; there is a dramatic difference in style here, because of the
;;; nesting, which convention helps to disambiguate.  pattern variables
;;; are marked with one question-mark prefix for each nested macro they
;;; occur in.  closure variables of syntactic continuations are marked
;;; with one asterisk suffix for each nested continuation they can
;;; occur in.  (in this case there is at most only one nested
;;; continuation.)
;;;
;;; assumptions:
;;;
;;; - the symbol `...' does not occur in any variable, reference
;;;   substitution, or assignment substitution.  it furthermore does
;;;   not occur anywhere in the expression; this restriction could be
;;;   lifted, though.
;;;
;;; - forms of the form (set! a b) occur only where you really mean
;;;   assignments.  we don't stop for quotations or anything, and we
;;;   don't care what macros you use here.  this is the real problem
;;;   with the mechanism -- it walks through the tree without regard
;;;   for its meaning.
;;;
;;; - vectors can mean only literals.

(define-syntax substitute-variables
  (syntax-rules (set!)

    ((substitute-variables
      ?continuation
      ((?variable ?reference-substitution ?assignment-substitution)
       ...)
      (set! ?lhs ?rhs))
     (let-syntax
         ((substitute
           (syntax-rules (?variable ...)
             ((substitute ??continuation ?variable ??rhs)
              (syntax-continue ??continuation
                               (?assignment-substitution ??rhs)))
             ...
             ((substitute ??continuation ??other ??rhs)
              (syntax-continue ??continuation (set! ??other ??rhs))))))
       (substitute ?continuation ?lhs ?rhs)))

    ((substitute-variables ?continuation ?substitutions (?a . ?d))
     (letrec-syntax
         ((continue-a
           (syntax-rules ()
             ((continue-a ??continuation* ??substitutions* ??d* ??new-a)
              (substitute-variables
               (continue-d ??continuation* ??substitutions* ??new-a)
               ??substitutions*
               ??d*))))
          (continue-d
           (syntax-rules ()
             ((continue-d ??continuation* ??substitutions* ??new-a* ??new-d)
              (syntax-continue ??continuation* (??new-a* . ??new-d))))))
       (substitute-variables
        (continue-a ?continuation ?substitutions ?d)
        ?substitutions
        ?a)))

    ((substitute-variables ?continuation ?substitutions #(?v ...))
     (syntax-continue ?continuation #(?v ...)))

    ((substitute-variables
      ?continuation
      ((?variable ?reference-substitution ?assignment-substitution)
       ...)
      ?datum)
     ;; this is not strictly speaking the best test; we ought to use
     ;; the bound identifier test.  but this is much conciser.  (the
     ;; same goes for the set! patterns above.)
     (let-syntax
         ((test
           (syntax-rules (?variable ...)
             ((test ??continuation ?variable)
              (syntax-continue ??continuation ?reference-substitution))
             ...
             ((test ??continuation ??other)
              (syntax-continue ??continuation ??other)))))
       (test ?continuation ?datum)))))