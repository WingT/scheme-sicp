(define-syntax when
  (syntax-rules ()
    ((when cexp sexp)
     (if cexp sexp))
    ((when cexp sexp aexp)
     (if cexp sexp aexp))))
(when true 1)
(when false (/ 1 0) 2)
