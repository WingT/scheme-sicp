(cd "/home/wing/Desktop/scheme/generic/")
(load "kernel.scm")
(load "complex-package.scm")
(load "generic-arith.scm")
(load "combine-types.scm")
;2.82
;the following is a correct version,however even if all the arguments are coerced to the same type,there can be no corresponding operations.
(define (coerce . args)
  (define (converse-iter args ans)
    (if (not args)
        false
        (if (null? args)
            ans
            (converse-iter (cdr args) (cons (car args) ans)))))
  (define (try-iter arg rest-args ans)
    (if (null? rest-args)
        ans
        (let ((a1 (car rest-args)))
          (let ((t1 (type-tag a1))
                (ta (type-tag arg)))
            (if (eq? t1 ta)
                (try-iter arg (cdr rest-args) (cons a1 ans))
                (let ((t1->ta (get-coercion t1 ta)))
                  (if t1->ta
                      (try-iter arg
                                (cdr rest-args)
                                (cons (t1->ta a1) ans))
                      false)))))))n
  (define (coerce-iter current-args)
    (if (null? current-args)
        false
        (let ((current-arg (car current-args)))
          (or (converse-iter (try-iter current-arg args '()) '())
              (coerce-iter (cdr current-args))))))
  (coerce-iter args))
;a better way is,see SICP-Solutions:
(define (apply-generic-1 op . args)
  (define (coerce-list-to-type lst type)
    (map (lambda (x)
           (let ((proc (get-coercion (type-tag x) type)))
             (if proc (proc x) x)))
         lst))
  (define (apply-coerced lst)
    (if (null? lst)
        (error
               "No method for these types -- APPLY-GENERIC"
               (list op (map type-tag args)))
        (let ((coerced-lst
               (coerce-list-to-type args (type-tag (car lst)))))
          (let ((proc (get op (map type-tag coerced-lst))))
            (if proc
                (apply proc (map contents coerced-lst))
                (apply-coerced (cdr lst)))))))
  (let ((types (map type-tag args)))
    (let ((proc (get op types)))
      (if proc
          (apply proc (map contents args))
          (apply-coerced args)))))
;better way for testing
(define (add-1 . args) (apply apply-generic-1 (cons 'add args)))
(put 'add '(complex complex complex complex complex)
     (lambda args
       (make-complex-from-real-imag
        (apply + (map real-part args))
        (apply + (map imag-part args)))))
(define a (make-complex-from-real-imag 1 2))
(define b (make-complex-from-real-imag 3 4))
(define c (make-complex-from-real-imag 5 6))
(define d (make-scheme-number 7))
(define e (make-scheme-number 8))
(add-1 a b c d e)
;2.84
(define (apply-generic-2 op . args)
  (define (higher? a1 a2)
    (let ((t1 (type-tag a1))
          (t2 (type-tag a2)))
      ;very easy to make mistake here
      (let ((raise1 (get 'raise (list t1))))
        (cond ((eq? t1 t2) false)
              (raise1 (higher? (raise a1) a2))
              (else true)))))
  (define (raise-arg-to-level arg level)
    (let ((type (type-tag arg)))
      (if (eq? type level)
          arg
          (raise-arg-to-level (raise arg) level))))
  (define (raise-to-same-level a1 a2)
    (if (higher? a1 a2)
        (list a1 (raise-arg-to-level a2 (type-tag a1)))
        (list (raise-arg-to-level a1 (type-tag a2)) a2)))
  (let ((types (map type-tag args)))
    (let ((proc (get op types)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((coerced-args
                     (apply raise-to-same-level args)))
                (let ((types (map type-tag coerced-args)))
                  (let ((proc (get op types)))
                    (if proc
                        (apply proc (map contents coerced-args))
                        (error
                         "No method for these types -- APPLY-GENERIC"
                         (list op types))))))
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op types)))))))
(define z (make-scheme-number 8))
(define w (make-rational 1 9))
(define x (make-real 8.9))
(define y (make-complex-from-real-imag 9 0))
(define u (make-complex-from-real-imag 9 0))
(define (add-2 x y) (apply-generic-2 'add x y))
(add-2 x y)
;2.84
(define (drop x)
  ())
(define x (make-complex-from-mag-ang 8 0))
(define y (make-complex-from-real-imag 8 0))
(define z (make-scheme-number 8))
(equ? x z)
(apply-generic 'equ? x z)
