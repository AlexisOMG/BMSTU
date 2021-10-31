(load "task2.scm")

(define (tree->scheme xs)
  (if (list? xs)
      (list (if (equal? (cadr xs) '^)
                'expt
                (cadr xs))
            (tree->scheme (car xs))
            (tree->scheme (caddr xs)))
      xs))

(tree->scheme (parse (tokenize "a + b/c^2 - d")))
(tree->scheme (parse (tokenize "x^(a + 1)")))
(eval (tree->scheme (parse (tokenize "2^2^2^2")))
      (interaction-environment))