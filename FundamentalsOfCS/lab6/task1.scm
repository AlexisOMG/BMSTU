;Fracs ::= Spaces Frac Spaces Fracs | .
;Spaces ::= Space Spaces | .
;Space ::= #\tab | #\newline | #\space .
;Frac ::= Int #\/ UnsignedInt .
;Int ::= #\+ UnsignedInt | #\- UnsignedInt | UnsignedInt .
;UnsignedInt ::= Digit | Digit UnsignedInt .
;Digit ::= #\0 | #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9 .

(define digit '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define space '(#\tab #\space #\newline))

(define (scan str)
  (letrec ((curVal (lambda () (car str)))
           (next (lambda () (set! str (cdr str))))
           (check-sign (lambda () (if (or (equal? (curVal) #\+)
                                          (equal? (curVal) #\-))
                                      (next))))
           (digit? (lambda () (member (curVal) digit)))
           (unsignedInt? (lambda () (and (digit?)
                                         (next)
                                         (if (and (not (null? str))
                                                  (digit?))
                                             (unsignedInt?)))))
           (int? (lambda () (check-sign) (unsignedInt?)))
           (check-slash (lambda () (equal? (curVal) #\/) (next))))
    (and (int?)
         (check-slash)
         (unsignedInt?)
         (null? str))))

(define (check-frac str)
  (scan (string->list str)))

(define (scan-frac str)
  (define xs (string->list str))
  (and (check-frac str)
       (let ((curVal (lambda () (car xs)))
             (next (lambda () (set! xs (cdr xs))))
             (finish (lambda () (null? xs))))
         (stream curVal next finish))))

(define (stream curVal next finish)
  (letrec ((digit? (lambda () (and (not (finish)) (member (curVal) digit))))
           (unsignedInt? (lambda (x)
                           (if (digit?)
                               (let ((n (- (char->integer (curVal))
                                           (char->integer #\0))))
                                 (next)
                                 (unsignedInt? (+ (* 10 x) n)))
                               x)))
           (check-sign (lambda ()
                         (cond
                           ((equal? (curVal) #\+) (begin (next) '+))
                           ((equal? (curVal) #\-) (begin (next) '-))
                           (else '+))))
           (Int? (lambda ()
                   (list (check-sign) (unsignedInt? 0))))
           (check-slash (lambda () (and (not (finish))
                                        (equal? (curVal) #\/)
                                        (next)
                                        '/))))
    (let ((rez (list (Int?)
                     (check-slash)
                     (unsignedInt? 0))))
      (let ((rez (list (cadr rez)
                       (car rez)
                       (caddr rez))))
        (and (finish)
             (not (member #f rez))
             (eval rez (interaction-environment)))))))

(define (scan-many-fracs str)
  (define xs (string->list str))
  (define ans '())
  (letrec ((curVal (lambda () (car xs)))
           (next (lambda () (set! xs (cdr xs))))
           (space? (lambda () (or (null? xs) (member (curVal) space))))
           (construct (lambda ()
                        (and (not (null? xs))
                             (if (space?)
                                 (next)
                                 (set! ans (cons (stream curVal next space?) ans)))
                             (construct)))))
    (begin
      (construct)
      (and (not (member #f ans)) (reverse ans)))))

(scan-many-fracs "\t1/2 1/3\n\n10/8")
(scan-many-fracs "\t1/2 1/3\n\n2/-5")

;(scan-frac "110/111")
;(scan-frac "-4/3")
;(scan-frac "+5/11")
;(scan-frac "5.0/10")
;(scan-frac "FF/10")

(load "unit-test.scm")

(define the-tests
  (list
   (test (check-frac "110/111") #t)
   (test (check-frac "-4/3") #t)
   (test (check-frac "+5/10") #t)
   (test (check-frac "5.0/10") #f)
   (test (check-frac "FF/10") #f)
   (test (scan-frac "110/111") (/ 110 111))
   (test (scan-frac "-4/3") (/ -4 3))
   (test (scan-frac "+5/11") (/ 5 11))
   (test (scan-frac "5.0/10") #f)
   (test (scan-frac "FF/10") #f)))

(run-tests the-tests)