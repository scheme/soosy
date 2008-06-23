(define-class <point> #f (x y))
(define-class <color-point> <point> (c))

(define pt  (make-object <point>))
(define cpt (make-object <color-point>))

(define (print x) (display x) (newline))

(print "Tests!")
(print (object-of-class? <point> pt))
(print (object-of-class? <color-point> cpt))
(print (object-of-class? <point> cpt))
(print (not (object-of-class? <color-point> pt)))