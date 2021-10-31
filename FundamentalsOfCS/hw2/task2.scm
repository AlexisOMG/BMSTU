(define (find n ns)
  (and (not (null? ns)) (or (= n (car ns)) (find n (cdr ns)))))

(define (list->set xs)
  (if (null? xs)
      '()
      (append
       (if (not (find (car xs) (cdr xs)))
           (list (car xs))
           '())
       (list->set (cdr xs)))))

(define (set? xs)
  (or (null? xs) (and (not (find (car xs) (cdr xs))) (set? (cdr xs)))))

(define (union xs ys)
  (list->set (append xs ys)))

(define (intersection xs ys)
  (define (do xs ys)
    (if (null? ys)
        '()
        (append
         (if (find (car ys) xs)
             (list (car ys))
             '())
         (do xs (cdr ys)))))
  (list->set (do xs ys)))

(define (difference xs ys)
  (if (null? xs)
      '()
      (append
       (if (find (car xs) ys)
           '()
           (list (car xs)))
       (difference (cdr xs) ys))))

(define (symmetric-difference xs ys)
  (append
   (difference xs ys)
   (difference ys xs)))

(define (set-eq? xs ys)
  (null? (symmetric-difference xs ys)))

(begin (display "(list->set '(1 1 2 3)) => ") (list->set '(1 1 2 3)))
(begin (display "\n(set? '(1 2 3)) => ") (set? '(1 2 3)))
(begin (display "(set? '(1 2 3 3)) => ") (set? '(1 2 3 3)))
(begin (display "(set? '()) => ") (set? '()))
(begin (display "\n(union '(1 2 3) '(2 3 4)) => ") (union '(1 2 3) '(2 3 4)))
(begin (display "\n(intersection '(1 2 3) '(2 3 4)) => ") (intersection '(1 2 3) '(2 3 4)))
(begin (display "\n(difference '(1 2 3 4 5) '(2 3)) => ") (difference '(1 2 3 4 5) '(2 3)))
(begin (display "\n(symmetric-difference '(1 2 3 4) '(3 4 5 6)) => ") (symmetric-difference '(1 2 3 4) '(3 4 5 6)))
(begin (display "\n(set-eq? '(1 2 3) '(1 2 3)) => ") (set-eq? '(1 2 3) '(1 2 3)))
(begin (display "(set-eq? '(1 2) '(1 3)) => ") (set-eq? '(1 2) '(1 3)))