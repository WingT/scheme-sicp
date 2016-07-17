;2.77
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
(define z (make-complex-from-real-imag 3 4))
(magnitude z)
;2.78
(define (type-tag-1 datum)
  (cond ((pair? datum) (car datum))
        ((symbol? datum) 'scheme-symbol)
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents-1 datum)
  (cond ((pair? datum) (cdr datum))
        ((or (symbol? datum) (number? datum)) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))
(define (attach-tag-1 type contents)
  (if (or (eq? type 'scheme-symbol) (eq? type 'scheme-number))
      contents
      (cons type contents)))
;2.79
(define (equ? x y) (apply-generic 'equ? x y))
