(define (run-let lis expr)
  (cond
    ((null? expr) '())
    ((list? (car expr)) (cons (run-let lis (car expr)) (run-let lis (cdr expr))))
    ((assoc (car expr) lis) (append (cdr (assoc (car expr) lis)) (run-let lis (cdr expr))))
    (else (cons (car expr) (run-let lis (cdr expr))))))

(define-syntax my-let
  (syntax-rules ()
    ((_ dict expr)
     (eval (run-let `dict `expr) (interaction-environment)))))

(define (dict l a)
  (cond
    ((null? l) '())
    ((not (number? (cadar l)))
     (if (list? (cadar l))
         (cons (dict (cadar l) a) (dict (cdr l) a))
         (cons (cons (caar l) (cdr (assoc (cadar l) a))) (dict (cdr l) a))))
    (else (cons (car l) (dict (cdr l) a)))))

(define-syntax my-let*
  (syntax-rules ()
    ((_ l expr)
     (eval (run-let (dict `l `l) `expr) (interaction-environment)))))


(my-let ((x 2) (y 3)) (my-let ((x 7) (z (+ x y))) (* z x)))

(my-let* ((x 2) (y x)) (my-let* ((z 8)) (/ z (+ (* x y) (+ x y)))))