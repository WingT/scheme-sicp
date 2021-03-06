(define (make-tree data l r)
  (list data l r))
(define (data-tree tree)
  (car tree))
(define (l-tree tree)
  (cadr tree))
(define (r-tree tree)
  (caddr tree))
(define (element-of-set? x set)
  (if (null? set)
      #f
      (let ((data (data-tree set)))
        (cond ((= data x) #t)
              ((< data x)
               (element-of-set? x (r-tree set)))
              (else (element-of-set? x (l-tree set)))))))
(define (adjoin-set x set)
  (if (null? set)
      (make-tree x '() '())
      (let ((data (data-tree set))
            (l (l-tree set))
            (r (r-tree set)))
        (cond ((= data x) set)
              ((< data x)
               (make-tree data l (adjoin-set x r)))
              (else
               (make-tree data (adjoin-set x l) r))))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (append list1 list2)
  (accumulate cons list2 list1))
(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (l-tree tree))
              (cons (data-tree tree)
                    (tree->list (r-tree tree))))))
;递归+迭代
(define (tree->list1 tree)
  (define (copy-to-list tree ans)
    (if (null? tree)
        ans
        (copy-to-list (l-tree tree)
                      (cons (data-tree tree)
                            (copy-to-list (r-tree tree) ans)))))
  (copy-to-list tree '()))
(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))
(define (list->tree elements)
  (partial-tree elements (length elements)))
(define (partial-tree elts n)
  (if (= n 0)
      (cons'() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result)))
            (let ((right-result
                   (partial-tree (cdr non-left-elts)
                                 (- n 1 left-size)))
                  (this-entry (car non-left-elts)))
              (cons (make-tree this-entry left-tree
                               (car right-result))
                    (cdr right-result))))))))
(define tree '(7 (3 (1 () ()) (5 () (3))) (9 () (11 () ()))))
(tree->list tree)
(tree->list1 tree)
(list->tree '(1 3 5 7 9 11))
