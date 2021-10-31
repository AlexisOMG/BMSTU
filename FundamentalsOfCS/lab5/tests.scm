(load "unit-test.scm")
(load "interpret.scm")

(define expr1 #(   define abs 
                    dup 0 < 
                    if neg endif 
                    end 
                    9 abs 
                    -9 abs      ))

(define expr2 #(   define =0? dup 0 = end
                    define <0? dup 0 < end
                    define signum
                    =0? if exit endif
                    <0? if drop -1 exit endif
                    drop
                    1
                    end
                    0 signum
                    -5 signum
                    10 signum       ))

(define expr3 #(   define -- 1 - end
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
                    4 factorial     ))

(define expr4 #(   define =0? dup 0 = end
                    define =1? dup 1 = end
                    define -- 1 - end
                    define fib
                    =0? if drop 0 exit endif
                    =1? if drop 1 exit endif
                    -- dup
                    -- fib
                    swap fib
                    +
                    end
                    define make-fib
                    dup 0 < if drop exit endif
                    dup fib
                    swap --
                    make-fib
                    end
                    10 make-fib     ))

(define expr5 #(   define =0? dup 0 = end
                    define gcd
                    =0? if drop exit endif
                    swap over mod
                    gcd
                    end
                    90 99 gcd
                    234 8100 gcd    ))

(define expr6 #(define a
                 1 2 = if
                 4 5 5 = if
                 2 +
                 endif
                 1 2 +
                 endif
                 end
                 a))

(define the-tests
  (list (test (interpret expr1 '()) '(9 9))
        (test (interpret expr2 '()) '(1 -1 0))
        (test (interpret expr3 '()) '(24 6 2 1 1))
        (test (interpret expr4 '()) '(0 1 1 2 3 5 8 13 21 34 55))
        (test (interpret expr5 '()) '(18 9))
        (test (interpret expr6 '()) '())))

(run-tests the-tests)