(define (read-words)
  (define (cin word)
    (let ((a (read-char)))
      (cond
        ((or (equal? a #\space) (equal? a #\newline) (equal? a #\tab))
         (if (= (string-length word) 0)
             (cin "")
             (cons word (cin ""))))
        ((eof-object? a)
         (if (= (string-length word) 0)
             '()
             (cons word '())))
        (else (cin (string-append word (string a)))))))
  (cin ""))

(read-words)