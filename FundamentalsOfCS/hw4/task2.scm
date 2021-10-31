(define-syntax lazy-cons
  (syntax-rules ()
    ((_ first second) (cons first (delay second)))))

(define lazy-car car)

(define (lazy-cdr lazy-pair)
  (force (cdr lazy-pair)))

(define (lazy-ref xs k)
  (if (zero? k)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (lazy-head xs k)
  (if (zero? k)
      '()
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))

(define (factorial)
  (define (next a b)
    (lazy-cons a (next (* a b) (+ b 1))))
  (next 1 2))

(define (lazy-factorials n)
  (lazy-ref (factorial) n))

(display (lazy-head (naturals 10) 12))
(newline)

(begin
  (display (lazy-factorials 10)) (newline)
  (display (lazy-factorials 50)) (newline))