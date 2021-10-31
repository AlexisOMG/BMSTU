(define-syntax for
  (syntax-rules (in as)
    ((_ x in xs expr ...)
     (let do ((x (car xs))
              (ath (cdr xs)))
       (if (null? ath)
           (begin expr ...)
           (begin expr ... (do (car ath) (cdr ath))))))
    ((_ xs as x expr ...)
     (for x in xs expr ...))))

(define len length)

(define-syntax define-data
  (syntax-rules ()
    ((_ name ()) (display "What are you doing???"))
    ((_ name ((type args ...) ...))
     (let ((pred (string->symbol (string-append (symbol->string 'name) "?")))
           (dict '()))
       (for xs in '((type args ...) ...)
            (eval `(define ,xs (list ,(symbol->string (car xs)) ,@(cdr xs))) (interaction-environment))
            (set! dict (cons (list (symbol->string (car xs)) (len xs)) dict)))
       (eval `(define (,pred obj)
                (and (list? obj)
                     (not (null? obj))
                     (not (equal? (member (list (car obj) (len obj)) ',dict) #f))))
             (interaction-environment))))))

(define-syntax match
  (syntax-rules ()
    ((_ x) #f)
    ((_ x ((type args ...) func) sim ...)
     (if (and (not (null? x))
              (list? x)
              (= (len x) (len '(type args ...))))
       (eval `((lambda ,'(args ...) ,'func) ,@(cdr x)) (interaction-environment))
       (match x sim ...)))))

; Определяем тип
;
(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

; Определяем значения типа
;
(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

; Пусть определение алгебраического типа вводит не только конструкторы, но и предикат этого типа:
;
(and (figure? s)
     (figure? r)
     (figure? t)
     (figure? c))



(define pi (acos -1)) ; Для окружности
  
(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))
  
(perim s)
(perim r)
(perim t)
   