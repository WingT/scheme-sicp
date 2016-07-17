(define (make-table)
  (let ((table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? (caar records) key) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (cdr table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key2 value)
                                  (cdr subtable)))))
            (set-cdr! table
                      (cons (list key1 (cons key2 value))
                            (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation"))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define put (operation-table 'insert!))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error
                          "No method for these types -- APPLY-GENERIC"
                          (list op type-tags))))))
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op type-tags))
          )))))
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup))
(define put-coercion (coercion-table 'insert!))
