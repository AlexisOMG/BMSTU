(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))))

(define (my-flatten xs)
  (if (null? xs)
      '()
      (append
       (if (list? (car xs))
           (my-flatten (car xs))
           (list (car xs)))
       (my-flatten (cdr xs)))))

(define (my-element? n xs)
  (and
   (not (null? xs))
   (or
    (= (car xs) n)
    (my-element? n (cdr xs)))))

(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (append
       (if (list? (car xs))
           (my-filter pred? (car xs))
           (if (pred? (car xs))
               (list (car xs))
               '()))
       (my-filter pred? (cdr xs)))))

(define (my-fold-left op xs)
  (define (left op xs)
    (if (null? (cdr xs))
        (car xs)
        (op (left op (cdr xs)) (car xs))))
  (left op (reverse xs)))

(define (my-fold-right op xs)
  (if (null? (cdr xs))
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

(begin (display "(my-range 0 11 3) => ") (my-range 0 11 3))
(begin (display "(my-flatten '((1) 2 (3 (4 5)) 6)) => ") (my-flatten '((1) 2 (3 (4 5)) 6)))
(begin (display "\n(my-element? 1 '(3 2 1)) => ") (my-element? 1 '(3 2 1)))
(begin (display "(my-element? 4 '(3 2 1)) => ") (my-element? 4 '(3 2 1)))
(begin (display "\n(my-filter odd? (my-range 0 10 1)) => ") (my-filter odd? (my-range 0 10 1)))
(begin (display "(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1)) => ")
       (my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1)))
(begin (display "\n(my-fold-left expt '(2 3 4)) => ") (my-fold-left expt '(2 3 4)))
(begin (display "(my-fold-left  quotient '(16 2 2 2 2)) => ") (my-fold-left  quotient '(16 2 2 2 2)))
(begin (display "\n(my-fold-right expt '(2 3 4)) => ") (my-fold-right expt '(2 3 4)))
(begin (display "(my-fold-right  quotient '(16 2 2 2 2)) => ") (my-fold-right  quotient '(16 2 2 2 2)))
