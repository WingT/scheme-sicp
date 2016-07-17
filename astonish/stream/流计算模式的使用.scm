;系统地将迭代操作方式表示为流过程
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                  (lambda (guess) (/ (+ guess (/ x guess)) 2))
                  guesses)))
  guesses)
(for-each (lambda (x)
            (newline)
            (display x))
          (show-n (sqrt-stream 2) 10))
;
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(define (display-n stream n)
  (for-each (lambda (x)
              (newline)
              (display x))
            (show-n stream n)))
(display-n pi-stream 9)
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 s2 (- (* 2 s1)))))
                 (euler-transform (stream-cdr s)))))
(display-n (euler-transform pi-stream) 10)
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
(display-n (accelerated-sequence euler-transform pi-stream) 8)
;3.63
(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s))))
         tolerance)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))
(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
;3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n) (scale-stream (ln2-summands (+ n 1)) -1)))
(define ln2-stream
  (partial-sums (ln2-summands 1)))
(display-n ln2-stream 10)
(display-n (accelerated-sequence euler-transform ln2-stream) 10)
