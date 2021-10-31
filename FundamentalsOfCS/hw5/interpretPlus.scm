(define (trueBool a) (if (equal? a #f) 0 1))

(define (schemeBool a) (not (= a 0)))

(define (len a)
  (if (vector? a)
      (vector-length a)
      (length a)))

(define (search s ind ws wc)
  (if (equal? s (vector-ref ws (+ wc ind)))
      ind
      (search s (+ ind 1) ws wc)))

(define (innerIf i ws wc)
  (cond
    ((= i 0) (- wc 1))
    ((equal? (vector-ref ws wc) 'if) (innerIf (+ i 1) ws (+ wc 1)))
    ((equal? (vector-ref ws wc) 'endif) (innerIf (- i 1) ws (+ wc 1)))
    ((and (= i 1) (equal? (vector-ref ws wc) 'else)) (innerIf (- i 1) ws (+ wc 1)))
    (else (innerIf i ws (+ wc 1)))))

(define (innerElse i ws wc)
  (cond
    ((= i 0) (- wc 1))
    ((equal? (vector-ref ws wc) 'if) (innerElse (+ i 1) ws (+ wc 1)))
    ((equal? (vector-ref ws wc) 'endif) (innerElse (- i 1) ws (+ wc 1)))
    (else (innerElse i ws (+ wc 1)))))

(define (innerWhile i ws wc)
  (cond
    ((<= i 0) wc)
    ((equal? (vector-ref ws wc) 'while) (innerWhile (+ i 1) ws (+ wc 1)))
    ((equal? (vector-ref ws wc) 'endwhile) (innerWhile (- i 1) ws (+ wc 1)))
    (else (innerWhile i ws (+ wc 1)))))

(define (innerEndwhile i ws wc)
  (cond
    ((>= i 0) (+ wc 1))
    ((equal? (vector-ref ws wc) 'while) (innerEndwhile (+ i 1) ws (- wc 1)))
    ((equal? (vector-ref ws wc) 'endwhile) (innerEndwhile (- i 1) ws (- wc 1)))
    (else (innerEndwhile i ws (- wc 1)))))

(define operations
  (list
   (cons '= (lambda (xs) (cons (trueBool (= (car xs) (cadr xs))) (cddr xs))))
   (cons '> (lambda (xs) (cons (trueBool (> (cadr xs) (car xs))) (cddr xs))))
   (cons '< (lambda (xs) (cons (trueBool (< (cadr xs) (car xs))) (cddr xs))))
   (cons 'not (lambda (xs) (cons (trueBool (not (schemeBool (car xs)))) (cdr xs))))
   (cons 'and (lambda (xs) (cons (trueBool (and (schemeBool (car xs)) (schemeBool (cadr xs)))) (cddr xs))))
   (cons 'or (lambda (xs) (cons (trueBool (or (schemeBool (car xs)) (schemeBool (cadr xs)))) (cddr xs))))
   (cons '+ (lambda (xs) (cons (+ (car xs) (cadr xs)) (cddr xs))))
   (cons '- (lambda (xs) (cons (- (cadr xs) (car xs)) (cddr xs))))
   (cons '/ (lambda (xs) (cons (quotient (cadr xs) (car xs)) (cddr xs))))
   (cons '* (lambda (xs) (cons (* (cadr xs) (car xs)) (cddr xs))))
   (cons 'neg (lambda (xs) (cons (- (car xs)) (cdr xs))))
   (cons 'mod (lambda (xs) (cons (remainder (cadr xs) (car xs)) (cddr xs))))
   (cons 'drop cdr)
   (cons 'swap (lambda (xs) (cons (cadr xs) (cons (car xs) (cddr xs)))))
   (cons 'dup (lambda (xs) (cons (car xs) xs)))
   (cons 'over (lambda (xs) (cons (cadr xs) xs)))
   (cons 'rot (lambda (xs) (cons (caddr xs) (cons (cadr xs) (cons (car xs) (cdddr xs))))))
   (cons 'depth (lambda (xs) (cons (len xs) xs)))))

(define (main ws wc xs rs as)
  (if (>= wc (len ws))
      xs
      (let ((word (vector-ref ws wc)))
        (cond
          ((number? word) (main ws (+ wc 1) (cons word xs) rs as))
          ((equal? word 'define)
           (main ws (+ wc (search 'end 0 ws wc) 1) xs rs (cons (cons (vector-ref ws (+ wc 1)) (+ wc 2)) as)))
          ((or (equal? word 'end) (equal? word 'exit))
           (main ws (car rs) xs (cdr rs) as))
          ((equal? word 'if)
           (if (schemeBool (car xs))
               (main ws (+ wc 1) (cdr xs) rs as)
               (main ws (+ (innerIf 1 ws (+ wc 1)) 1) (cdr xs) rs as)))
          ((equal? word 'else)
           (main ws (+ (innerElse 1 ws (+ wc 1)) 1) xs rs as))
          ((equal? word 'endif)
           (main ws (+ wc 1) xs rs as))
          ((equal? word 'while)
           (if (schemeBool (car xs))
               (main ws (+ wc 1) (cdr xs) rs as)
               (main ws (innerWhile 1 ws (+ wc 1)) (cdr xs) rs as)))
          ((equal? word 'endwhile)
           (main ws (innerEndwhile -1 ws (- wc 1)) xs rs as))
          ((assoc word as)
           (main ws (cdr (assoc word as)) xs (cons (+ wc 1) rs) as))
          ((assoc word operations)
           (main ws (+ wc 1) ((cdr (assoc word operations)) xs) rs as))
          (else (begin (display "ERRRRRROOOORRRRRRR\n")
                       (main ws (len ws) '() '() '())))))))

(define (interpret ws xs)
  (main ws 0 xs (list (len ws)) '()))


