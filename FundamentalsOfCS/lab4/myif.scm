(define-syntax my-if
  (syntax-rules ()
    ((_ con f s)
     (let ((condition con)
           (first (delay f))
           (second (delay s)))
       (or (and condition (force first)) (and (not condition) (force second)))))))

(my-if (= 1 1) 1 (/ 1 0))
(my-if #f (/ 1 0) 1)