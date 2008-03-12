;;; -*- Mode: Scheme; scheme48-package: soosy -*-
;;;
;;; A class-based single-dispatch OO system with generic functions
;;;

(define-record-type* class
  (%make-class name superclass (variables) methods)
  ())

(define-record-discloser :class
  (lambda (c)
    `(Class ,(class-name c) ,(class-variables c))))

(define-record-type* object
  (%make-object class variables)
  ())

(define-record-discloser :object
  (lambda (obj)
    `(Object ,(object-class obj) ,(object-variables obj))))


(define (make-class name superclass variables)
  (let ((entry         (assq name *class-descriptors*))
        (all-variables (if superclass
                           (append (class-variables superclass) variables)
                           variables)))
    (let ((make-class
	   (lambda ()
             (%make-class name
                          superclass
                          all-variables
                          (if superclass
                              (class-methods superclass)
                              '())))))
      (if (not entry)
	  (let ((class (make-class)))
	    (set! *class-descriptors* (cons (cons name class) *class-descriptors*))
	    class)
	  (let ((class (cdr entry)))
	    (cond ((not (eq? (class-superclass class) superclass))
		   (let ((class (make-class)))
		     (set-cdr! entry class)
		     class))
		  ((equal? all-variables (class-variables class))
		   class)
		  (else
		   (warn "Redefining class:" name)
		   (set-class-variables! class variables)
		   class)))))))

(define (class-method class name)
  (class-methods/ref (class-methods class) name))

(define (class-methods/ref methods name)
  (or (method-lookup methods name)
      (error "unknown method" name)))

(define (method-lookup methods name)
  (let loop ((methods methods))
    (and methods
	 (let ((entry (assq name (car methods))))
	   (if entry
	       (cdr entry)
	       (loop (cdr methods)))))))

(define (class-method-define class name method)
  (let ((methods (class-methods class)))
    (let ((entry (assq name (car methods))))
      (if entry
	  (set-cdr! entry method)
	  (set-car! methods (cons (cons name method) (car methods))))))
  name)

(define (base-class? class)
  (eq? class #f))

(define (subclass? class class*)
  (or (eq? class class*)
      (let loop ((class (class-superclass class)))
	(and class
	     (or (eq? class class*)
		 (loop (class-superclass class)))))))

(define (make-object class)
  (if (not (class? class))
      (error "wrong type argument" class)
      (%make-object class
                    (make-vector (length (class-variables class))))))

(define (object-of-class? class object)
  (and (object? object)
       (or (base-class? class)
           (eq? class (object-class object))
           (object-of-class? (class-superclass class) object))))

(define (object-methods object)
  (class-methods (object-class object)))