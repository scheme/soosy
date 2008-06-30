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

(define (set-x! pt new-x)
  (with-instance-variables <point> pt (x)
    (set! x new-x)))

(define (set-y! pt new-y)
  (with-instance-variables <point> pt (y)
    (set! y new-y)))

(define-method <point> :set-x set-x!)
(define-method <point> :set-y set-y!)
