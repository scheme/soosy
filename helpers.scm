;;; -*- Mode: Scheme; scheme48-package: soosy-helpers -*-

;; '((name class-record) ...)
(define *class-descriptors* '())
;; '((generic-id lambda) ...)
(define *generic-functions* '())

(define *generic-function-counter* 0)

(define (name->class name)
  (if (false? name)
      name
      (lookup name *class-descriptors* "unknown class")))

(define (generic-functions-ref id)
  (lookup id *generic-functions*
          "unknown generic function"))

(define (lookup key store message)
  (let ((entry (assq key store)))
    (if (not entry)
        (warn message key)
        (cdr entry))))

(define (false? x) (not x))

(define-syntax make-getters
  (lambda (form rename compare)
    (if (not (= 3 (length form)))
        (syntax-error "make-getters instance variables")
        (let* ((instance    (second form))
               (variables   (third form))
               (var-count   (length variables))
               (%variables  (rename 'object-variables))
               (%vector-ref (rename 'vector-ref)))
          (let loop ((index   0)
                     (vars    variables)
                     (getters '()))
            (if (= index var-count)
                getters
                (loop (+ 1 index)
                      (cdr vars)
                      (cons `(,(first vars)
                              (,%vector-ref (,%variables ,instance) ,index))
                            getters))))))))

(define (make-setters instance variables rename)
  (let ((var-count     (length variables))
        (%set!         (rename 'set!))
        (%syntax-rules (rename 'syntax-rules))
        (%variables    (rename 'object-variables))
        (%vector-set!  (rename 'vector-set!)))
    (let loop ((index 0)
               (vars variables)
               (setters '()))
      (if (= index var-count)
          `(,%syntax-rules ,variables
                           ,@setters
                           ((set! variable value)
                            (,%set! variable value)))
          (loop (+ 1 index)
                (cdr vars)
                (cons `((set! ,(first vars) value)
                        (,%vector-set!
                         (,%variables ,instance) ,index value))
                      setters))))))
