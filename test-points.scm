(define-class <point> #f (x y))
(define-class <color-point> <point> (c))

(define pt (make-object <point>))

#|
We want the following code

(with-instance-variables pt (x y)
    (set! x 4)
    (set! x 5)
    (list x y))

to expand into

(let-syntax ((set! (syntax-rules (x y)
                     ((set! x value)
                      (vector-set! (object-variables pt) 0 value))
                     ((set! y value)
                      (vector-set! (object-variables pt) 1 value))
                     ((set! variable value)
                      (%set! variable value))))) ; %set! is the renamed set!
  (let ((x (vector-ref (object-variables pt) 0))
        (y (vector-ref (object-variables pt) 1)))
    (set! x 4)
    (set! x 5)
    (list x y)))

|#