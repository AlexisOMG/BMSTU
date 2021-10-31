(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (not (list? xs))
          (list xs)
          (append
           (if (list? (car xs))
               (my-flatten (car xs))
               (list (car xs)))
           (my-flatten (cdr xs)) ))))

(define (list-del xs n)
  (append (reverse (list-tail (reverse xs) (- (length xs) n)))
          (list-tail xs (+ n 1))))

(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (append
       (if (pred? (car xs))
           (list (car xs))
           '() )
       (my-filter pred? (cdr xs)) ) ))

(define (my-element? x xs)
  (and (not (null? xs))
       (or (equal? x (car xs))
           (my-element? x (cdr xs)))))

(define (have-element pred? xs)
  (define (h-v pred? xs n)
    (cond
      ((null? xs) -1)
      ((pred? (car xs)) n)
      (else (h-v pred? (cdr xs) (+ n 1)))))
  (h-v pred? xs 0))

(define (my-element-smart? x xs)
  (my-element? x (my-flatten xs)))

(define (const? expr)
  (not (my-element-smart? 'x expr)))

(define e (exp 1))

(define (one? n) (= n 1))

(define (my-ev xs) (eval xs (interaction-environment)))

(define (smart-calc op start expr xs)
  (cond
    ((null? expr) (if (= (op start start) start)
                      xs
                      (cons start xs)))
    ((number? (car expr))
     (smart-calc op (op start (car expr)) (cdr expr) xs))
    (else (smart-calc op start (cdr expr)
                      (append xs (list (car expr)))))))

(define (smart-const? xs)
  (and (const? xs)
       (not (my-element-smart? 'log xs))
       (not (my-element-smart? 'sin xs))
       (not (my-element-smart? 'cos xs))
       (not (my-element-smart? 'e xs))))

;==============================================================

(define (normalize xs)
  (cond
    ((number? xs) xs)
    ((and (list? xs) (= (length xs) 1)) (normalize (car xs)))
    ((smart-const? xs) (my-ev xs))
    ((and (list? xs)
          (equal? '+ (car xs)))
     (let ((a (my-filter (lambda (x) (or (not (number? x))
                                         (not (zero? x))))
                         (map normalize (cdr xs)))))
       (cond
         ((= (length a) 0) 0)
         ((= (length a) 1) (car a))
         (else `(+ ,@a)))))
    ((and (list? xs)
          (equal? '- (car xs)))
     (if (and (number? (normalize (cadr xs)))
              (zero? (normalize (cadr xs))))
         (normalize `(+ ,@(cdr (cdr xs))))
         (let ((a (my-filter (lambda (x) (or (not (number? x))
                                             (not (zero? x))))
                             (map normalize (cdr xs)))))
           (cond
             ((= (length a) 0) 0)
             ((= (length a) 1)
              (if (smart-const? (car a))
                  (- (car a))
                  `(- ,(car a))))
             (else `(- ,@a))))))
    ((and (list? xs)
          (equal? '* (car xs)))
     (let ((a (my-filter (lambda (x) (or (not (number? x))
                                         (not (one? x))))
                         (map normalize (cdr xs)))))
       (cond
         ((my-element-smart? 0 a) 0)
         ((= (length a) 0) 1)
         ((= (length a) 1) (car a))
         (else (let ((n (have-element
                         (lambda (x)
                           (and (list? x)
                                (equal? '* (car x)))) a)))
                 (if (>= n 0)
                     `(* ,@(smart-calc
                            * 1
                            (normalize
                             (append (list-del a n)
                                     (cdr (list-ref a n))))
                            '()))
                     `(* ,@(smart-calc * 1 a '()))))))))
    ((and (list? xs)
          (equal? 'expt (car xs)))
     (let ((a (map normalize (cdr xs))))
       (cond
         ((and (number? (list-ref a 1)) (= (list-ref a 1) 1))
          (list-ref a 0))
         ((and (number? (list-ref a 1)) (= (list-ref a 1) 0))
          0)
         ((equal? 'e (list-ref a 0)) `(exp ,(list-ref a 1)))
         (else `(ex
                 pt ,@a)))))
    ((list? xs) `(,(car xs) ,@(map normalize (cdr xs))))
    (else xs)))

