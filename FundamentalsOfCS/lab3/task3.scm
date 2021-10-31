(define (list-insert! xs ind el)
  (if (= ind 0)
      (cons el xs)
      (cons (car xs) (list-insert! (cdr xs) (- ind 1) el))))

(define (ref xs i . n)
  (if (null? n)
      (or
        (and (list? xs) (>= i 0) (< i (length xs)) (list-ref xs i))
        (and (vector? xs) (>= i 0) (< i (vector-length xs)) (vector-ref xs i))
        (and (string? xs) (>= i 0) (< i (string-length xs)) (string-ref xs i)))
      (or
        (and (list? xs) (>= i 0) (<= i (length xs)) (list-insert! xs i (car n)))
        (and (vector? xs) (>= i 0) (<= i (vector-length xs)) (list->vector (list-insert! (vector->list xs) i (car n))))
        (and (string? xs) (>= i 0) (<= i (string-length xs)) (char? (car n)) (list->string (list-insert! (string->list xs) i (car n)))))
      ))

(load "test.scm")

(define the-tests
  (list (test (ref '(1 2 3) 1) 2)
        (test (ref #(1 2 3) 1) 2)
        (test (ref "123" 1) #\2)
        (test (ref "123" 3) #f)
        (test (ref '(1 2 3) 1 0) '(1 0 2 3))
        (test (ref #(1 2 3) 1 0) '#(1 0 2 3))
        (test (ref #(1 2 3) 1 #\0) '#(1 #\0 2 3))
        (test (ref "123" 1 #\0) "1023")
        (test (ref "123" 1 0) #f)
        (test (ref "123" 3 #\4) "1234")
        (test (ref "123" 5 #\4) #f)))

(run-tests the-tests)
