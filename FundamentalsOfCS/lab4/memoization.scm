(define trib (let ((a1 '()))
               (lambda (n)
                 (cond
                   ((assoc n a1) (cdr (assoc n a1)))
                   ((<= n 1) 0)
                   ((= n 2) 1)
                   (else (begin (set! a1 (cons (cons n (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3)))) a1))
                                (cdar a1)))))))