;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .

(define escape #f)

(define (parsing prog)
  (and (call-with-current-continuation
        (lambda (cc) (set! escape cc)))
       (letrec ((curVal (lambda () (and (not (null? prog)) (car prog))))
                (next (lambda () (set! prog (cdr prog))))
                (program (lambda () (list (articles) (body))))
                (articles (lambda () (let ((art (article)))
                                       (if art
                                           (cons art (articles))
                                           '()))))
                (article (lambda ()
                           (if (equal? (curVal) 'define)
                               (begin (next)
                                      (let ((name (curVal)))
                                        (next)
                                        (let ((bdy (body)))
                                          (if (equal? (curVal) 'end)
                                              (begin (next)
                                                     (list name bdy))
                                              (escape #f)))))
                               #f)))
                (body (lambda ()
                        (cond 
                          ((equal? (curVal) 'if)
                           (begin (next)
                                  (let ((bdy (body)))
                                    (if (equal? (curVal) 'endif)
                                        (begin
                                          (next)
                                          (cons (list 'if bdy) (body)))
                                        (escape #f)))))
                          ((curVal) (let ((cur (curVal)))
                                      (if (or (equal? cur 'end) (equal? cur 'endif))
                                          '()
                                          (begin (next)
                                                 (cons cur (body))))))
                          (else '())))))
         (program))))

(define (parse program)
  (parsing (vector->list program)))

(parse #(1 2 +))
(parse #(x dup 0 swap if drop -1 endif))
(parse #( define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if drop 1 exit endif
              =1? if drop 1 exit endif
              dup --
              factorial
              *
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial ))
(parse #(define word w1 w2 w3))