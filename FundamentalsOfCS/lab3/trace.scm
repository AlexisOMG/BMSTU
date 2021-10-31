(define-syntax trace-ex
  (syntax-rules ()
    ((_ x)
     (begin
       (display 'x)
       (display " => ")
       (let ((f x))
         (display f)
         (newline)
         f)))))