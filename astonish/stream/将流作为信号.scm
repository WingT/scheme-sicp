(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
;3.73
(define (RC r c dt)
  (lambda (i v0)
    (add-streams (integral (scale-stream i (/ 1 c)) v0 dt)
                 (scale-stream i r))))
(define RC1 (RC 5 1 0.001))
(display-n (RC1 ones 1) 10)
3.74
(define (make-stream seq)
  (if (null? seq)
      the-empty-stream
      (cons-stream (car seq) (make-stream (cdr seq)))))
(define the-sense-data
  (make-stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))
(define (sign-change-detector current-value last-value)
  (cond ((and (< last-value 0) (> current-value 0)) 1)
        ((and (> last-value 0) (< current-value 0)) -1)
        (else 0)))
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))
(define zero-crossings (make-zero-crossings the-sense-data 0))
(display-n zero-crossings 12)
(define zero-crossings
  (stream-map sign-change-detector the-sense-data
              (cons-stream 0 the-sense-data)))
(display-n zero-crossings 13)
