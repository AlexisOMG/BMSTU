(define (cut str)
  (if (not (or (equal? (car str) #\space) (equal? (car str) #\newline) (equal? (car str) #\tab)))
      str
      (cut (cdr str))))

(define (string-trim-left str)
  (list->string (cut (string->list str))))

(define (string-trim-right str)
  (list->string (reverse (cut (reverse (string->list str))))))

(define (string-trim str)
  (string-trim-left (string-trim-right str)))

(define (search a b)
  (or (null? a) (and (not (null? b)) (equal? (car a) (car b)) (search (cdr a) (cdr b)))))

(define (string-prefix? a b)
  (search (string->list a) (string->list b)))

(define (string-suffix? a b)
  (search (reverse (string->list a)) (reverse (string->list b))))

(define (string-infix? a b)
  (define (do a b)
    (and (not (or (null? a) (null? b))) (or (string-prefix? (list->string a) (list->string b)) (do a (cdr b)))))
  (do (string->list a) (string->list b)))

(define (string-split str sep)
  (define (skip st i)
    (if (and (> i 0) (not (null? st)))
        (skip (cdr st) (- i 1))
        st))
  (define (do s p rez tmp)
    (if (null? s)
        (if (null? tmp)
            rez
            (append rez (list (list->string tmp))))
        (if (not (string-prefix? (list->string p) (list->string s)))
            (do (cdr s) p rez (append tmp (list (car s))))
            (do (skip s (length p))
              p
              (if (null? tmp) rez (append rez (list (list->string tmp))))
              '()))))
  (do (string->list str) (string->list sep) '() '()))

(begin (display "(string-trim-left \"\\tabs def\") => ") (string-trim-left "\tabs def"))
(begin (display "(string-trim-right \"abs def\\t\") => ") (string-trim-right "abs def\t"))
(begin (display "(string-trim \"\\t   abs def  \\t\") => ") (string-trim "\t   abs def  \t"))
(begin (display "\n(string-prefix? \"abc\" \"abcdef\") => ") (string-prefix? "abc" "abcdef"))
(begin (display "(string-prefix? \"bcd\" \"abcdef\") => ") (string-prefix? "bcd" "abcdef"))
(begin (display "\n(string-suffix? \"def\" \"abcdef\") => ") (string-suffix? "def" "abcdef"))
(begin (display "(string-suffix? \"bcd\" \"abcdef\") => ") (string-suffix? "bcd" "abcdef"))
(begin (display "\n(string-infix? \"def\" \"abcdefgh\") => ") (string-infix? "def" "abcdefgh"))
(begin (display "(string-infix? \"abc\" \"abcdefgh\") => ") (string-infix? "abc" "abcdefgh"))
(begin (display "(string-infix? \"fgh\" \"abcdefgh\") => ") (string-infix? "fgh" "abcdefgh"))
(begin (display "(string-infix? \"ijk\" \"abcdefgh\") => ") (string-infix? "ijk" "abcdefgh"))
(begin (display "\n(string-split \"x;y;z\" \";\") => ") (string-split "x;y;z" ";"))
(begin (display "(string-split \"x-->y-->z\" \"-->\") => ") (string-split "x-->y-->z" "-->"))