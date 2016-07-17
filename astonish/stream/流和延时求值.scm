;书上的let并不是必须的
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream
                               (force delayed-integrand)
                               dt)
                              int)))
  int)
(define (solve f y0 dt)
  (define y (integral (delay dy/dt) y0 dt))
  (define dy/dt (stream-map f y))
  y)
(define the-result (solve (lambda (y) y) 1 0.00001))
;100000*0.00001=1
(stream-ref the-result 100000)
;3.77这样实现较慢,里面的delay既是为了满足递归调用的参数形式相同，同时也是为了满足延时计算过程，以solve为例，它保证了在计算出dy的下一项之后才来计算y的下一项，不然会发生y的下一项是the-empty-stream之类的情况
(define (integral-2 delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral-2 (delay (stream-cdr integrand))
                               (+ (* (stream-car integrand)
                                     dt)
                                  initial-value)
                               dt)))))
;3.78讲道理dy/dt是唯一的，并不需要做为参数给出来，不知道什么意思
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy/dt) y0 dt))
  (define dy/dt (integral (delay ddy/dtt) dy0 dt))
  (define ddy/dtt (add-streams
                   (scale-stream dy/dt a)
                   (scale-stream y b)))
  y)
;下式的解是e^x+e^(3x),x=1时值是22.80381875
(define the-integral (solve-2nd 4 -3 0.001 2 4))
(stream-ref the-integral 1000)
;3.79
(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy/dt) y0 dt))
  (define dy/dt (integral (delay ddy/dtt) dy0 dt))
  (define ddy/dtt (f dy/dt y))
  y)
(define the-integral (solve-2nd (lambda (dy/dt y)
                                  (add-streams
                                   (scale-stream dy/dt 4)
                                   (scale-stream y -3)))
                                0.001 2 4))
(stream-ref the-integral 1000)
;3.80
(define (RLC r l c dt)
  (lambda (vc0 il0)
    (define vc
      (integral (delay (scale-stream il (/ -1 c))) vc0 dt))
    (define il
      (integral (delay (add-streams
                        (scale-stream vc (/ 1 l))
                        (scale-stream il (/ (- r) l))))
                il0 dt))
    (cons vc il)))
(define the-rlc (RLC 1 1 0.2 0.1))
(define the-v-l (the-rlc 10 0))
(stream-ref (car the-v-l) 10)
(stream-ref (cdr the-v-l) 10)
;for-proof方程转化为i''=-i'-5i,v'=-5i,前者可用上面的解二阶微分方程的程序解
(define the-i (solve-2nd (lambda (dy/dt y)
                           (add-streams
                            (scale-stream dy/dt -1)
                            (scale-stream y -5)))
                         0.1 0 10-0))
(define the-v (integral (delay (scale-stream the-i -5)) 10 0.1))
(stream-ref the-i 10)
(stream-ref the-v 10)
