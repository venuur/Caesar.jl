(define account 0)

(define deposit
  (lambda (amt)
    (set! account (+ account amt))))

(define withdraw
  (lambda (amt)
    (set! account (- account amt))))

(display "BALANCE: ")
(display account)
(newline)

(deposit 20)
(display "BALANCE: ")
(display account)
(newline)

(withdraw 10)
(display "BALANCE: ")
(display account)
(newline)
