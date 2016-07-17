(define random-init 1)
(define (rand-update x)
  (remainder (+ (* 13 x) 5) 24))
(define (random-numbers op-stream)
  (define (numbers last-value op-stream)
    (let ((op (stream-car op-stream)))
      (cond ((pair? op)
             (cons-stream (cadr op)
                          (numbers (cadr op)
                                   (stream-cdr op-stream))))
            (else (let ((next-value (rand-update last-value)))
                    (cons-stream next-value
                                 (numbers next-value
                                          (stream-cdr op-stream))))))))
  (numbers random-init op-stream))
(define the-op-stream (list->stream '(generate (reset 9))))
(define the-random-numbers (random-numbers the-op-stream))
;要查看流中的全部元素，不能用“流.scm”里定义的show-n，因为最后只有一个元素的时候，op-stream是空，但还会调用numbers过程，所以需要利用延时求值
(define (show-n-2 s n)
  (if (= n 0)
      '()
      (let ((the-s (force s)))
        (cons (stream-car the-s)
              (show-n-2 (delay (stream-cdr the-s)) (- n 1))))))
(show-n-2 (delay the-random-numbers) 2)
;参考sicp-solution隐式定义的版本:
(define (random-numbers op-stream)
  (define numbers
    (cons-stream random-init
                 (stream-map
                  (lambda (num op)
                    (if (pair? op)
                        (cadr op)
                        (rand-update num)))
                  numbers op-stream)))
  (stream-cdr numbers))
(define the-random-numbers (random-numbers the-op-stream))
(show-n-2 (delay the-random-numbers) 2)
