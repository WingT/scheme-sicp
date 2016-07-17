;leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (symbol-leaf leaf)
  (cadr leaf))
(define (weight-leaf leaf)
  (caddr leaf))
(define (leaf? obj)
  (eq? 'leaf (car obj)))
;tree
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (append x y)
  (accumulate cons y x))
(define (make-tree left right)
  (list left right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))
(define (symbols obj)
  (if (leaf? obj)
      (list (symbol-leaf obj))
      (caddr obj)))
(define (weight obj)
  (if (leaf? obj)
      (weight-leaf obj)
      (cadddr obj)))
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
;decode
(define (decode bits tree)
  (define (decode1 bits tmp)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) tmp)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode1 (cdr bits) tree))
              (decode1 (cdr bits) next-branch)))))
  (decode1 bits tree))
(define (choose-branch bit branch)
  (cond ((= 0 bit) (left-branch branch))
        ((= 1 bit) (right-branch branch))))
;2.68 encode
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (encode-in current)
    (if (leaf? current)
        (if (eq? symbol (symbol-leaf current))
            '(#t)
            '(#f))
        (let ((right-result (encode-in (right-branch current))))
          (if (car right-result)
              (cons #t (cons 1 (cdr right-result)))
              (let ((left-result
                     (encode-in
                      (left-branch current))))
                (if (car left-result)
                    (cons #t (cons 0 (cdr left-result)))
                    '(#f)))))))
  (let ((result (encode-in tree)))
    (if (car result)
        (cdr result)
        (error "no match symbol!"))))
;; (define (encode-symbol symbol tree)
;;   (define (encode-symbol1 tmp ans)
;;     (if (leaf? tmp)
;;         (if (eq? symbol (symbol-leaf tmp))
;;             ans
;;             #f)
;;         (let ((left-result
;;                (encode-symbol1
;;                 (left-branch tmp)
;;                 (append ans '(0)))))
;;           (if left-result
;;               left-result
;;               (encode-symbol1 (right-branch tmp) (append ans '(1)))))))
;;   (let ((result (encode-symbol1 tree '())))
;;     (if result
;;         result
;;         (error "no match symbol"))))
;insert&sort
(define (adjoin-set item set)
  (if (null? set)
      (list item)
      (cond ((> (weight (car set)) (weight item))
             (cons item set))
            (else (cons (car set) (adjoin-set item (cdr set)))))))
(define (make-leaf-set pairs);o(n^2)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))
;;2.69
(define (generate-huffman-tree pairs);o(n^2)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs)
  (define (make-code-tree pairs)
    (cond ((or (null? pairs) (null? (cdr pairs))) pairs)
          (else (adjoin-set
                 (make-tree (car pairs) (cadr pairs))
                 (cddr pairs)))))
  (define (successive-merge-iter pairs)
    (cond ((null? pairs) '())
          ((null? (cdr pairs)) (car pairs))
          (else (successive-merge-iter (make-code-tree pairs)))))
  (successive-merge-iter pairs))
