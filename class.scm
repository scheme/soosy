;;; -*- Mode: Scheme; scheme48-package: soosy -*-
;;;
;;; A class-based single-dispatch OO system with generic functions
;;;

(define-record-type* class
  (%make-class %name superclass (subclasses) (variables) methods)
  ())

(define-record-discloser :class
  (lambda (c)
    `(Class ,(class-name c)
            superclass: ,(class-name (class-superclass c))
            subclasses: ,(map class-name (class-subclasses c))
            variables:  ,(class-variables c)
            methods:    ,(disclose-methods (class-methods c)))))

(define-record-type* object
  (%make-object class variables)
  ())

(define-record-discloser :object
  (lambda (obj)
    `(Object ,(class-name (object-class obj)) ,(disclose-variables obj))))

(define (disclose-variables object)
    (map (lambda (name value) `(,name -> ,value))
         (class-variables (object-class object))
         (vector->list (object-variables object))))

(define (disclose-methods methods)
  (hash-table-keys methods))

(define (make-class name superclass variables)
  (let ((class         (name->class name))
        (all-variables (if superclass
                           (append (class-variables superclass) variables)
                           variables))
        (subclasses    '()))
    (let ((make-class
	   (lambda ()
             (%make-class name
                          superclass
                          subclasses
                          all-variables
                          (if superclass
                              (hash-table-copy (class-methods superclass))
                              (make-hash-table))))))
      (if (not class)
          ;; if class has not been defined before
	  (let ((class (make-class)))
	    (add-class! name class)
            (class-add-subclass! superclass class)
	    class)
          ;; if the given superclass does not match the known superclass
          (cond ((not (eq? (class-superclass class) superclass))
                 (class-remove-subclass! (class-superclass class) class)
                 (let ((class (make-class)))
                   (add-class! name class)
                   (class-add-subclass! superclass class)
                   class))
                (else class))))))

(define (class-add-subclass! class subclass)
  (if (class? class)
      (set-class-subclasses! class (cons subclass (class-subclasses class)))))

(define (class-remove-subclass! class subclass)
  (if (class? class)
      (set-class-subclasses! class (delete subclass (class-subclasses class)))))

(define (class-method class name)
  (class-methods/ref (class-methods class) name))

(define (class-name class)
  (if (class? class)
      (class-%name class)
      class))

(define (class-methods/ref methods name)
  (or (method-lookup methods name)
      (error "unknown method" name)))

(define (method-lookup methods name)
  (hash-table-ref/default methods name #f))

(define (class-method-define class name method)
  (cond
   ((not (class?     class))  (error "not a class" class))
   ((not (procedure? method)) (error "not a procedure" method))
   (else
    (let ((methods (class-methods class)))
      (hash-table-set! methods name method)
      (map (lambda (subclass)
             (let ((methods (class-methods subclass)))
               (hash-table-set! methods name method)))
           (class-subclasses class)))))
  name)

(define (base-class? class)
  (eq? (class-superclass class) #f))

(define (subclass? class class*)
  (and (class? class)
       (class? class*)
       (or (eq? class class*)
           (subclass? class (class-superclass class*)))))

(define (make-object class)
  (if (not (class? class))
      (error "wrong type argument" class)
      (%make-object class
                    (make-vector (length (class-variables class))))))

(define (object-of-class? class object)
  (and (object? object)
       (class? class)
       (subclass? class (object-class object))))

(define (object-methods object)
  (if (object? object)
      (class-methods (object-class object))
      #f))

(define (object-method object name)
  (if (object? object)
      (class-method (object-class object) name)
      #f))

(define (offset-of variable class)
  (list-index (lambda (item) (eq? item variable))
              (class-variables class)))

(define (object-variable object variable)
  (let* ((variables (object-variables object))
         (class     (object-class object))
         (offset    (offset-of variable class)))
    (vector-ref variables offset)))

(define (set-object-variable! object variable value)
  (let* ((variables (object-variables object))
         (class     (object-class object))
         (offset    (offset-of variable class)))
    (vector-set! variables offset value)))

(define (send object operation . args)
  (let ((method (object-method object operation)))
    (apply method object args)))

(define (usual-method class name)
  (class-method (class-superclass class) name))

(define (send-usual object operation . args)
  (apply (usual-method (object-class object) operation) object args))
