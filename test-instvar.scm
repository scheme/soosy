(define-class <point> #f (x y))

(define (make-point x y)
  (let ((pt (make-object <point>)))
    (set-object-variable! pt 'x x)
    (set-object-variable! pt 'y y)
    pt))

(define pt (make-point 0 0))

(define (get-x pt)
  (with-instance-variables <point> pt (x) x))

(define (get-y pt)
  (with-instance-variables <point> pt (y) y))

(define-method <point> :x get-x)
(define-method <point> :x get-y)

;; (define (set-x! pt new-x)
;;   (with-instance-variables <point> pt (x)
;;     (set! x new-x)))

;; (define-method <point> :set-x set-x!)

;;; This is the expansion
(define (set-x! pt new-x)
  (let* ((class    <point>)
         (instance pt)
         (x        (object-variable instance 'x))
         (y        (object-variable instance 'y)))
    (let-syntax
        ((%set! (syntax-rules () ((%set! old new) (set! old new)))))
      (let-syntax
        ((set!
          (syntax-rules (x y)
            ((set! x value)
             (begin (set-object-variable! instance 'x value)
                    (%set! x value)))
            ((set! y value)
             (begin (set-object-variable! instance 'y value)
                    (%set! y value)))
            ((set! local value)
             (%set! local value)))))
        (set! x new-x)
        (display x) (newline)
        (set! x (+ 1 new-x))))))
