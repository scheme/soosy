;;; -*- Mode: Scheme; scheme48-package: soosy -*-
;;;
;;; A class-based single-dispatch OO system with generic functions
;;;

(define-record-type* class
  (%make-class name superclass (object-size) (variables) methods)
  ())

(define-record-type* object
  (%make-object class variables)
  ())

(define (make-class name superclass variables)
  (let ((entry       (assq name *class-descriptors*))
        (object-size (+ (length variables)
                        (if superclass (class-object-size superclass) 1))))
    (let ((make-class
	   (lambda ()
             (%make-class name
                          superclass
                          object-size
                          ;; store instvars in a vector
                          variables
                          (cons '() (and superclass
                                         (class-methods superclass)))))))
      (if (not entry)
	  (let ((class (make-class)))
	    (set! *class-descriptors* (cons (cons name class) *class-descriptors*))
	    class)
	  (let ((class (cdr entry)))
	    (cond ((not (eq? (class-superclass class) superclass))
		   (let ((class (make-class)))
		     (set-cdr! entry class)
		     class))
		  ((and (= object-size (class-object-size class))
			(equal? variables (class-variables class)))
		   class)
		  (else
		   (warn "Redefining class:" name)
		   (set-class-object-size! class object-size)
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
                    (make-vector (class-object-size class)))))

(define (object-of-class? class object)
  (and (object? object)
       (eq? class (object-class object))))

(define (object-methods object)
  (class-methods (object-class object)))