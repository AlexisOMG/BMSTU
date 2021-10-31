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

(define (set-ref! xs ind x)
  (if (= ind 0)
      (cons x (cdr xs))
      (cons (car xs) (set-ref! (cdr xs) (- ind 1) x))))

(define-syntax define-struct
  (syntax-rules ()
    ((_ name) (display "Why did you do it?:|"))
    ((_ name (args ...))
     (let ((type (symbol->string 'name))
           (pred (string->symbol (string-append (symbol->string 'name) "?")))
           (make (string->symbol (string-append "make-" (symbol->string 'name)))))
       (eval `(define (,pred whats)
                (and (list? whats)
                     (= (length '(args ...)) (- (length whats) 1))
                     (equal? (car whats) ,type)))
             (interaction-environment))
       (eval `(define (,make args ...) (list ,type args ...)) (interaction-environment))
       (define i 1)
       (for '(args ...) as arg
         (let ((ref (string->symbol (string-append type "-" (symbol->string arg))))
               (set (string->symbol (string-append "set-" type "-" (symbol->string arg) "!"))))
           (eval `(define (,ref xs) (list-ref xs ,i))
                 (interaction-environment))
           (eval `(define-syntax ,set
                    (syntax-rules ()
                      ((_ xs a)
                       (set! xs (set-ref! xs ,i a)))))
                 (interaction-environment)))
         (set! i (+ i 1)))))))


(define-struct pos (row col)) ; Объявление типа pos
(define p (make-pos 1 2))     ; Создание значения типа pos

(write p)
(newline)

(pos? p)

(pos-row p) 
(pos-col p)

(set-pos-row! p 3) ; Изменение значения в поле row
(set-pos-col! p 4) ; Изменение значения в поле col

(pos-row p)
(pos-col p)