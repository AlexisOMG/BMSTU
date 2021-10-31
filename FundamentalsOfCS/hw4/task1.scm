(define memoized-factorial
  (let ((a1 '()))
    (lambda (n)
      (cond
        ((assoc n a1) (cdr (assoc n a1)))
        ((= n 0) 1)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (begin (set! a1 (cons (cons n (* n (memoized-factorial (- n 1)))) a1)) (cdar a1)))))))

(begin
  (display (memoized-factorial 10)) (newline)
  (display (memoized-factorial 50)) (newline))