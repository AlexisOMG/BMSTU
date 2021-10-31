(define-syntax test
  (syntax-rules ()
    ((_ expr e)
     (list (quote expr) e))))

(define (run-test the-test)
  (let ((expr (car the-test)))
    (write expr)
    (let* ((res (eval expr (interaction-environment)))
           (status (equal? (cadr the-test) res)))
      (if status
          (display " ok")
          (display " FAIL"))
      (newline)
      (if (not status)
          (begin
            (display " Expected: ")
            (write (cadr the-test))
            (newline)
            (display " Returned: ")
            (write res)
            (newline)))
      status)))

(define (and-fold x xs)
  (if (null? xs)
      x
      (and-fold (and x (car xs)) (cdr xs))))

(define (run-tests the-tests)
  (and-fold #t (map run-test the-tests)))