(define (const? expr)
  (or (null? expr)
      (if (list? expr)
          (and (const? (car expr)) (const? (cdr expr)))
          (not (equal? expr 'x)))))


(define (derivative expr)
  (cond
    ((const? expr) 0)
    ((equal? expr 'x) 1)
    ((equal? expr '(- x)) -1)
    ((equal? (car expr) '+) `(+ ,@(map derivative (cdr expr))))
    ((equal? (car expr) '-) `(- ,@(map derivative (cdr expr))))
    ((equal? (car expr) '*) `(+ (* ,(derivative (cadr expr)) ,@(cddr expr)) (* ,(cadr expr) ,(derivative (append '(*) (cddr expr))))))
    ((equal? (car expr) '/) `(/ (- (* ,(derivative (cadr expr)) ,@(cddr expr)) (* ,(cadr expr) ,(derivative (append '(*) (cddr expr))))) (* ,@(cddr expr) ,@(cddr expr))))
    ((equal? (car expr) 'expt) (cond
                                 ((equal? (cadr expr) 'e) `(* ,(derivative (caddr expr)) (exp ,(caddr expr))))
                                 ((const? (caddr expr)) `(* ,(derivative (cadr expr)) ,(caddr expr) (expt ,(cadr expr) (- ,(caddr expr) 1))))
                                 ((const? (cadr expr)) `(* ,expr (log ,(cadr expr)) ,(derivative (caddr expr))))
                                 (else `(* ,expr ,(derivative `(* (log ,(cadr expr)) ,(caddr expr)))))))
    ((equal? (car expr) 'exp) (derivative `(expt e ,(cadr expr))))
    ((equal? (car expr) 'log) `(/ ,(derivative (cadr expr)) ,(cadr expr)))
    ((equal? (car expr) 'cos) `(* -1 ,(derivative (cadr expr)) (sin ,(cadr expr))))
    ((equal? (car expr) 'sin) `(* ,(derivative (cadr expr)) (cos ,(cadr expr))))))

(load "test.scm")

(define expr (eval `(lambda (x) ,(derivative '(+ x 2))) (interaction-environment)))

(define the-tests
  (list (test (expr 2) 1)))

(run-tests the-tests)













