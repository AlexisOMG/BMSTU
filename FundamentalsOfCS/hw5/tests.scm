(load "unit-test.scm")
(load "interpretPlus.scm")

(define expr1 #(     define =? over = end
                      define >? over < end
                      define <? over > end
                      define range
                      0 =? if exit endif
                      0 >? if dup 1 - range
                      else dup 1 + range endif
                      end
                      range))

(define expr2 #(     define bit-number
                      dup 1 > while
                      dup 2 mod swap 2 /
                      dup 1 > endwhile end
                      bit-number))

(define expr3 #(     define >? over < end
                      0 swap 1 >? while
                      2 / swap 1 +
                      swap 1 >? endwhile
                      drop))

(define expr4 #(     dup 0 > if
                         dup 1 +
                         dup 9 > if
                         dup 3 + else
                         dup 3 - endif else
                         dup 1 + endif))

(define expr5 #(     define f
                      dup 2 + g
                      end
                      define g
                      dup 1 < if
                      exit
                      else
                      dup 6 - f
                      end
                      f))

(define expr6 #(    define a
                     1 2 = if
                     4 5 5 = if
                     2 +
                     endif
                     1 2 +
                     endif
                     end a))

(define the-tests
  (list (test (interpret expr1 '(10)) '(0 1 2 3 4 5 6 7 8 9 10))
        (test (interpret expr1 '(-10)) '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10))
        (test (interpret expr2 '(10)) '(1 0 1 0))
        (test (interpret expr2 '(255)) '(1 1 1 1 1 1 1 1))
        (test (interpret expr3 '(1)) '(0))
        (test (interpret expr3 '(1024)) '(10))
        (test (interpret expr4 '(5)) '(3 6 5))
        (test (interpret expr4 '(-5)) '(-4 -5))
        (test (interpret expr5 '(20)) '(-2 -4 2 0 6 4 10 8 14 12 18 16 22 20))
        (test (interpret expr6 '()) '())))

(run-tests the-tests)