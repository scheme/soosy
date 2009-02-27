(define-class <point> #f (x y))
(define-class <color-point> <point> (c))

(define pt  (make-object <point>))
(define cpt (make-object <color-point>))

(define (point-x pt)
  (with-instance-variables <pt> pt (x)
      x))

(define (set-point-x! pt new-x)
  (with-instance-variables <pt> pt (x)
    (set! x new-x)))

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