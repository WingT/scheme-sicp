;Arithmetic
(define var 0)
(define f (lambda (x) (set! var (+ var 1)) var))
(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define succ (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))
(define plus (lambda (m n) (lambda (f) (lambda (x) ((n f) ((m f) x))))))
;(define plus (lambda (m n) ((m succ) n)))
(define mult (lambda (m n) (lambda (f) (n (m f)))))
;(define mult (lambda (m n) ((m (lambda (x) (plus n x))) zero)))
(define pow (lambda (b e) (e b)))
(define pred
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (((n (lambda (g)
                (lambda (h) (h (g f)))))
          (lambda (u) x))
         (lambda (u) u))))))
(define sub (lambda (m n) ((n pred) m)))
(set! var 0)
;Logic and predicates
(define true (lambda (x) (lambda (y) x)))
(define false (lambda (x) (lambda (y) y)))
(define and (lambda (p) (lambda (q) ((p q) p))))
(define or (lambda (p) (lambda (q) ((p p) q))))
(define not (lambda (p) ((p false) true)))
(define ifthenelse (lambda (p a b) ((p a) b)))
(define iszero (lambda (n) ((n (lambda (x) false)) true)))
(define leq (lambda (m n) (iszero (sub m n))))
;; (define pred (lambda (n)
;;                (((n (lambda (g)
;;                       (lambda (k)
;;                         (((iszero (g one))
;;                           K)
;;                          (plus (g k) one)))))
;;                  (lambda (v) zero))
;;                 zero)))
;pairs
(define cons (lambda (x y) (lambda (f) ((f x) y))))
(define car (lambda (p) (p true)))
(define cdr (lambda (p) (p false)))
(define nil (lambda (x) true))
(define null? (lambda (p) (p (lambda (x) (lambda (y) false)))))
;; (define pred (lambda (n) (car ((n (lambda (x)
;;                                      (cons (cdr x)
;;                                            (succ (cdr x)))))
;;                                (cons zero zero)))))
;recursion and fixed points
(define g (lambda (r n)
            (if (= n 0) 1 (* n (r r (- n 1))))))
(define fac (lambda (x) (g g x)))
(fac 6)
((lambda (x n) (x x n))
 (lambda (r n)
   (if (= n 0) 1 (* n (r r (- n 1)))))
 6)
(define y (lambda (g) ((lambda (x) (g (x x)))
                       (lambda (x) (g (x x))))))
(define g (lambda (r)
            (lambda (n)
              (if (= n 0) 1 (* n (r (- n 1)))))))
((y g) 6)
(define (omega x) (x x))
(define g (lambda (r)
            (lambda (n)
              (if (= n 0) 1 (* n ((r r) (- n 1)))))))
((omega g) 6)
((g g) 6)
