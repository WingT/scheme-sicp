;append
((lambda (x y)
   ((lambda (append)
      (append append x y))
    (lambda (append x y)
      (if (null? x) y (cons (car x) (append append (cdr x) y))))))
 '(1 2 3) '(4 5 6))
