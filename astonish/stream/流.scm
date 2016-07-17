;流实现的行为方式
;delay和force的实现
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if already-run?
          result
          (begin
            (set! already-run? true)
            (set! result (proc))
            result)))))
;(delay exp)应该等价于下面这个式子,但不能用define,目前不知道该如何绑定
(memo-proc (lambda () exp))
(define (test-func)
  (define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2)))))
  (fib 30))
(define promise (memo-proc (lambda () (test-func))))
(define (my-force delayed-project)
  (delayed-project))
;分别求值下面两个表达式，比较速度
(my-force promise)
(my-force promise)
;3.50
(define (my-stream-map proc . argstreams)
  (if (empty-stream? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply my-stream-map
              (cons proc
                    (map stream-cdr argstreams))))))
(define a (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define b (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define c (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
(define (display-stream stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons (stream-car stream)
            (display-stream (stream-cdr stream)))))
(display-stream (my-stream-map + a b c))
;3.51
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ 1 low) high))))
(define (display-line x)
  (newline)
  (display x))
(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
;3.52
(define (stream-filter pred? stream)
  (cond ((empty-stream? stream)
         the-empty-stream)
        ((pred? (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred? (stream-cdr stream))))
        (else (stream-filter pred? (stream-cdr stream)))))
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)
;无穷流
(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define no-sevens
  (stream-filter
   (lambda (x) (not (= (remainder x 7) 0))) integers))
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define (divsible? x y)
  (= (remainder x y) 0))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve
    (stream-filter
     (lambda (x)
       (not (divsible? x (stream-car stream))))
     (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
;隐式地定义流
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1  (scale-stream double 2)))
(define primes (cons-stream 2
                            (stream-filter prime?
                                           (integers-starting-from
                                            3))))
(define (prime? x)
  (define (iter ps)
    (let ((p (stream-car ps)))
      (cond ((> (square p) x) true)
            ((= (remainder x p) 0) false)
            (else (iter (stream-cdr ps))))))
  (iter primes))
;3.53
(define s (cons-stream 1 (add-streams s s)))
(stream-ref s 8)
;3.54
(define (mul-streams s1 s2)
  (cons-stream
   (* (stream-car s1)
      (stream-car s2))
   (mul-streams
    (stream-cdr s1)
    (stream-cdr s2))))
(define factorials
  (cons-stream 1 (mul-streams factorials
                              (integers-starting-from 2))))
;3.55
(define (partial-sums s)
  (define the-partial-sums
    (cons-stream (stream-car s)
                 (add-streams (stream-cdr s) the-partial-sums)))
  the-partial-sums)
;nice and elegant,相当于从相反的方向构建
(define (partial-sums-2 s)
  (define the-partial-sums
    (add-streams s (cons-stream 0 the-partial-sums)))
  the-partial-sums)
(define z (partial-sums-2 integers))
;3.56
(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                       (cons-stream s1car
                        (merge (stream-cdr s1) s2)))
                      ((= s1car s2car)
                       (cons-stream s1car
                        (merge (stream-cdr s1)
                               (stream-cdr s2))))
                      (else
                       (cons-stream s2car
                                    (merge s1 (stream-cdr s2)))))))))
(define s (cons-stream 1 (merge (scale-stream s 2)
                                (merge (scale-stream s 3)
                                       (scale-stream s 5)))))
;3.58这个过程求的是radix进制下(num/den)的小数部分,需假设num<den
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
(define z (expand 1 7 10))
(define (show-n s n)
  (if (= n 0)
      '()
      (cons (stream-car s)
            (show-n (stream-cdr s) (- n 1)))))
(show-n z 6)
(show-n z 12)
;3.59
;;a
(define (integrate-series s)
  (stream-map / s integers))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(show-n exp-series 8)
;;b
(define cosine-series
  (cons-stream 1
               (scale-stream
                (integrate-series sine-series)
                -1)))
(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))
(show-n cosine-series 9)
(show-n sine-series 9)
;3.60
(define (mul-series s1 s2);
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2)
                              (stream-car s1))
                (mul-series (stream-cdr s1) s2))))
(define (mul-series-2 s1 s2);o(n^2)
  (add-streams
   (scale-stream s2 (stream-car s1))
   (cons-stream
    0
    (mul-series-2 (stream-cdr s1) s2))))

(define sine*cosine (mul-series sine-series cosine-series))
(show-n sine*cosine 10)
;3.61
(define (1/S s)
  (define the-1/S
    (cons-stream 1
                 (mul-series (scale-stream (stream-cdr s) -1)
                             the-1/S)))
  the-1/S)
;warning:下面这个版本直接用递归，由于每次调用1/S时都会产生一个新的流，所以在计算过程中，记忆功能实际上并没有用到，但是算过一次后，两者的表现是一样的，读者可以分别运行两个版本多次比较差别
(define (1/S s)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s) -1)
                           (1/S s))))
;3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "div-series: divided by zero!")
      (mul-series s1 (1/S s2))))
(define tan-series (div-series sine-series cosine-series))
(show-n tan-series 9)
;
(define (display-n stream n)
  (for-each (lambda (x)
              (newline)
              (display x))
            (show-n stream n)))
