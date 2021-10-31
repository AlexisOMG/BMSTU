(define-syntax when
  (syntax-rules ()
    ((_ cond expr ...)
     (if cond (begin expr ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond expr ...)
     (when (not cond) expr ...))))

(define-syntax for
  (syntax-rules (in as)
    ((_ x in xs expr ...)
     (let do ((x (car xs))
              (ath (cdr xs)))
       (if (null? ath)
           (begin expr ...)
           (begin expr ... (do (car ath) (cdr ath))))))
    ((_ xs as x expr ...)
     (for x in xs expr ...))))

(define-syntax while
  (syntax-rules ()
    ((_ cond expr ...)
     (let do () (if cond (begin expr ... (do)))))))

(define-syntax repeat
  (syntax-rules (until)
    ((_ expr ... until cond)
     (let do ()
       (begin expr ... (if (not cond) (do)))))))

(define endl "\n")
(define-syntax cout
  (syntax-rules (<<)
    ((_ << x ath ...)
     (begin (display x) (cout ath ...)))
    ((_) (display ""))))

(define-syntax use-assertions
  (syntax-rules ()
    ((_)
     (call-with-current-continuation (lambda (cc) (set! contin cc))))))

(define contin #f)

(define-syntax assert
  (syntax-rules ()
    ((_ cond)
     (if (not cond)
         (begin (display "FAILED:")
                (display 'cond)
                (and (contin (newline))))))))

;;;;;;;;;;;;;;;;;EXAMPLES;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x 1)
(when (> x 0) (display "x > 0") (newline))
(newline)
(unless (= x 0) (display "x != 0") (newline))
(newline)

(for i in '(8 11 47) (display i) (newline))
(newline)

(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))

(newline)
(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))

(newline)
(define x 1)
(repeat (display x) (newline) (set! x (+ x 1)) until (< x 10))

(let ((i 0)
      (j 0))
  (repeat (set! j 0)
          (repeat (display (list i j))
                  (set! j (+ j 1))
                  until (= j 3))
          (set! i (+ i 1))
          (newline)
          until (= i 3)))

(cout << endl)
(cout << "a = " << 1 << endl << "b = " << 2 << endl)
(cout << endl)

(use-assertions)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))

(map 1/x '(1 2 3 4 5))

(map 1/x '(-2 -1 0 1 2))

