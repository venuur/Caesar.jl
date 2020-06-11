(define acc-fact
  (lambda (n acc)
    (if (eq? n 0)
      acc
      (acc-fact (- n 1) (* n acc)))))

(define fact
  (lambda (n)
    (acc-fact n 1)))

(display (fact 5))
(newline)
