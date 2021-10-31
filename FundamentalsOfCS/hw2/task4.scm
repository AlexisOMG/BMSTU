(define (size xs sz)
  (if (null? xs)
      sz
      (size (cdr xs) (* sz (car xs)))))

(define (make-multi-vector sizes . fill)
  (if (null? fill)
      (cons sizes (make-vector (size sizes 1)))
      (cons sizes (make-vector (size sizes 1) fill))))

(define (multi-vector? m)
  (and (pair? m) (not (null? (car m))) (= (vector-length (cdr m)) (size (car m) 1))))

(define (ind indexes size sizes i div)
    (if (null? indexes)
        i
        (ind (cdr indexes) size (cdr sizes) (+ (/ (* size (car indexes)) (* div (car sizes))) i) (* div (car sizes)))))

(define (multi-vector-ref m indices)
  (vector-ref (cdr m) (ind indices (size (car m) 1) (car m) 0 1)))

(define (multi-vector-set! m indices x)
  (vector-set! (cdr m) (ind indices (size (car m) 1) (car m) 0 1) x))

(begin (display "(define m (make-multi-vector '(11 12 9 16)))") (define m (make-multi-vector '(11 12 9 16))))
(begin (display "\n(multi-vector? m) => ") (multi-vector? m))
(begin (display "(multi-vector-set! m '(10 7 6 12) 'test)") (multi-vector-set! m '(10 7 6 12) 'test))
(begin (display "\n(multi-vector-ref m '(10 7 6 12)) => ") (multi-vector-ref m '(10 7 6 12)))
(begin (display "(define m (make-multi-vector '(3 5 7) -1))") (define m (make-multi-vector '(3 5 7) -1)))
(begin (display "(multi-vector-ref m '(0 0 0)) => ") (multi-vector-ref m '(0 0 0)))