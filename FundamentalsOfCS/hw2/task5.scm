(define (o . xs)
  (if (null? xs)
      (lambda (a) a)
      (lambda (a) ((car xs) ((apply o (cdr xs)) a)))))

(define (f x) (* x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(and (display "((o f g h) 1) => ") ((o f g h) 1))
(and (display "((o f g) 1) => ") ((o f g) 1))
(and (display "((o h) 1) => ") ((o h) 1))
(and (display "((o) 1) => ") ((o) 1))

(define (f x) (+ x 3))
(define (g x) (* x 2))
(define (h x) (- x))

(and (display "((o f g h) 2) => ") ((o f g h) 2))