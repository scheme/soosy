(define-class <point> #f (x y))
(define-class <color-point> <point> (c))

(define pt  (make-object <point>))
(define cpt (make-object <color-point>))

(define (point-x pt)
  (with-instance-variables <point> pt (x) x))

(define (set-point-x! pt new-x)
  (with-instance-variables <point> pt (x)
    (set-ivar! x new-x)))

(define (point-y pt)
  (with-instance-variables <point> pt (y) y))

(define (set-point-y! pt new-y)
  (with-instance-variables <point> pt (y)
    (set-ivar! y new-y)))

(define (make-point x y)
  (let ((pt (make-object <point>)))
    (set-point-x! pt x)
    (set-point-y! pt y)
    pt))

(define (print x) (display x) (newline))
(define (hello x) (print "Hello!"))
(define-method <point> :hello hello)

(==> pt :hello)
(define (say this word) (print word))
(define-method <point> :say say)
(==> pt :say "hi")
(==> cpt :say "subclass")

(class-method-define <color-point> ':say (lambda (this word) (print (string-append "WORD: " word))))

(define-method <color-point> (:say point message) (print (string-append "word: " message)))

(==> cpt :say "override")
(usual==> cpt :say "usual")

(print "Tests!")
(print (object-of-class? pt  <point>))
(print (object-of-class? cpt <color-point>))
(print (object-of-class? pt  <point>))
(print (not (object-of-class? pt <color-point>)))

(define pt (make-point 10 20))
(print pt)

(define z 0)

(with-instance-variables <point> pt (x y)
    (set-ivar! x 20)
    (print "**Should say use SET-IVAR! instead**")
    (set! x 20)
    (set! z 10)
    (print "**Should say z is not an ivar**")
    (set-ivar! z 10))

(print (= z 10))
