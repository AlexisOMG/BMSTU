(load "task3.scm")

(define (save-data data path)
  (with-output-to-file path (lambda () (write data))))

(define (load-data path)
  (with-input-from-file path (lambda () (read))))

(define (count path)
  (define (my-read str)
    (let ((char (read-char)))
      (if (or (eof-object? char) (equal? char #\newline))
          (cons char str)
          (my-read (cons char str)))))
  (define (counting cnt)
    (let ((line (my-read '())))
      (cond
        ((eof-object? (car line)) cnt)
        ((equal? (string-trim-left (list->string line)) "") (counting cnt))
        (else (counting (+ cnt 1))))))
  (with-input-from-file path (lambda () (counting 0))))

(count "test.txt")
(load-data "ggwp.txt")