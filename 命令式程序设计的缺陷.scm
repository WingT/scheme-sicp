(define (factorial n)
  (define (iter tmp ans)
    (if (> tmp n)
        ans
        (iter (+ 1 tmp) (* ans tmp))))
  (iter 1 1))
(define (factorial-1 n)
  (let ((ans 1)
        (tmp 1))
    (define (iter)
      (if (> tmp n)
          ans
          (begin
            (set! ans (* ans tmp))
            (set! tmp (+ 1 tmp))
            (iter))))
    (iter)));(iter)必须要在let表达式里面
(define (factorial-2 n)
  (define ans 1)
  (define tmp 1)
  (define (iter)
      (if (> tmp n)
          ans
          (begin
            (set! ans (* ans tmp))
            (set! tmp (+ 1 tmp))
            (iter))))
  (iter))
;3.7
(define (make-secret-account balance password)
  (define (withdraw account)
    (if (>= balance account)
        (begin (set! balance (- balance account))
               balance)
        "insufficient funds"))
  (define (deposit account)
    (set! balance (+ balance account))
    balance)
  (lambda (n m)
    (cond ((not (eq? n password))
           (lambda (x) "wrong password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit))))
;通过对比取0元的返回结果和错误信息判断密码是否正确
(define (make-joint acc old-pas new-pas)
  (if (not (equal? ((acc old-pas 'withdraw) 0) "wrong password"))
      (lambda (n m)
        (cond ((not (eq? n new-pas))
               (lambda (x) "wrong password"))
              (else (acc old-pas m))))
      (display "wrong password!")))
;3.8
(define f
  (let ((0-first #f))
    (lambda (x)
      (cond ((= x 0) (set! 0-first #t)))
      (if 0-first
          0
          x))))
;
(define f
  (lambda (first)
    (set! f (lambda (x) 0))
    first))
