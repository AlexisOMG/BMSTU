(define (a^n+-b^n? expr)
  (and (list? expr)
       (= (length expr) 3)
       (or (equal? '- (list-ref expr 0))
           (equal? '+ (list-ref expr 0)))
       (equal? 'expt (list-ref (cadr expr) 0))
       (equal? 'expt (list-ref (caddr expr) 0))
       (= (list-ref (cadr expr) 2)
          (list-ref (caddr expr) 2))))

(define (a^n+-b^n expr)
  (cond
    ((equal? (list '- 2) (list (list-ref expr 0) (list-ref (cadr expr) 2)))
     `(* (- ,(list-ref (cadr expr) 1) ,(list-ref (caddr expr) 1))
         (+ ,(list-ref (cadr expr) 1) ,(list-ref (caddr expr) 1))))
    ((equal? (list '- 3) (list (list-ref expr 0) (list-ref (cadr expr) 2)))
     `(* (- ,(list-ref (cadr expr) 1) ,(list-ref (caddr expr) 1))
         (+ (expt ,(list-ref (cadr expr) 1) 2)
            (* ,(list-ref (cadr expr) 1) ,(list-ref (caddr expr) 1))
            (expt ,(list-ref (caddr expr) 0) 2))))
    ((equal? (list '+ 3) (list (list-ref expr 0) (list-ref (cadr expr) 2)))
     `(* (+ ,(list-ref (cadr expr) 1) ,(list-ref (caddr expr) 1))
         (+ (expt ,(list-ref (cadr expr) 1) 2)
            (- (* ,(list-ref (cadr expr) 1) ,(list-ref (caddr expr) 1)))
            (expt ,(list-ref (caddr expr) 1) 2))))))

(define (factorize expr)
  (and
   (a^n+-b^n? expr)
   (a^n+-b^n expr)))

(load "test.scm")

(define the-tests
  (list (test (factorize '(- (expt x 2) (expt y 2)))
              '(* (- x y) (+ x y)))
        (test (factorize '(- (expt x 3) (expt y 3)))
              '(* (- x y) (+ (expt x 2) (* x y) (expt expt 2))))
        (test (factorize '(+ (expt x 3) (expt y 3)))
              '(* (+ x y) (+ (expt x 2) (- (* x y)) (expt y 2))))
        (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
              '(* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))
        (test (eval (list (list 'lambda '(x y) (factorize '(- (expt x 2) (expt y 2)))) 1 2) (interaction-environment))
              -3)))

(run-tests the-tests)