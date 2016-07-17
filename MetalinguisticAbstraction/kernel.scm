;4.1.1
;eval
(define apply-in-underlying-scheme apply)
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
;4.6
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ;4.20
        ((letrec? exp) (eval (letrec->combination exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
;apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
;procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
;conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
;sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
;assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)
;4.1
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (cons left
              (list-of-values-lr (rest-operands exps) env)))))
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-rl (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              right))))
;4.1.2
(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false
      (cadddr exp)))
;bug?
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (rest-seq seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
;procedure application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;derived expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest))))))
;4.3
(load "table.scm")
(define (eval-1 exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'exp (car exp)) ((get 'exp (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp))
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))
(put 'exp 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'exp 'set! eval-assignment)
(put 'exp 'define eval-definition)
(put 'exp 'if eval-if)
(put 'exp 'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
                       (lambda-body exp)
                       env)))
(put 'exp 'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))
(put 'exp 'cond
     (lambda (exp env) (eval (cond->if exp) exp)))
;4.4
;special form
(define (no-exps? exps) (null? exps))
(define (eval-and exp env)
  (define (eval-and-iter exps)
    (cond ((no-exps? exps) 'true)
          ((false? (eval (first-exp exps) env)) 'false)
          (else (eval-and-iter (rest-exps exps)))))
  (eval-and-iter (operands exp)))
(put 'exp 'and eval-and)
(define (eval-or exp env)
  (define (eval-or-iter exps)
    (cond ((no-exps? exps) 'false)
          ((true? (eval (first-exp exps) env)) 'true)
          (else (eval-or-iter (rest-exps exps)))))
  (eval-or-iter (operands exp)))
(put 'exp 'or eval-or)
;derived form
(define (and->if exp)
  (expand-and-clauses (cdr exp)))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))
(define (eval-and-2 exp env)
  (eval-if (and->if exp) env))

(define (or->if exp)
  (expand-or-clauses (cdr exp)))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-clauses (cdr clauses)))))
(define (eval-or-2 exp env)
  (eval-if (or->if exp) env))
;4.5
(define (expand-clauses-2 clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (eq? (cadr first) '=>)
                (cons (make-lambda
                       '(x)
                       (list (make-if 'x
                                  (list (caddr first) 'x)
                                  (expand-clauses-2 rest))))
                      (list (cond-predicate first)))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses-2 rest)))))))
;4.6
(define (let? exp)
  (tagged-list? exp 'let))
(define (let->combination exp)
  (cons
   (make-lambda
    (map car (cadr exp))
    (cddr exp))
   (map cadr (cadr exp))))
;4.7
(define (let*? exp)
  (tagged-list? exp 'let*))
(define (make-let bindings body)
  (cons 'let (cons bindings body)))
(define (let*->nested-lets exp)
  (define (rec bindings)
    (cond ((null? bindings) (cddr exp))
          ((last-exp? bindings)
           (make-let (list (car bindings))
                     (cddr exp)))
          (else (make-let
                 (list (car bindings))
                 (list (rec (cdr bindings)))))))
  (rec (cadr exp)))
;4.8
;;the example can be transformed into the following form:
;; (define (fib n)
;;   (let ((a 1)
;;         (b 0)
;;         (count n))
;;       (define (fib-iter a b count)
;;         (if (= count 0)
;;             b
;;             (fib-iter (+ a b) a (- count 1))))
;;       (fib-iter a b count)))
(define (make-definition head body)
  (cons 'define (cons head body)))
(define (let->combination-2 exp)
  (if (pair? (cadr exp))
      (cons
       (make-lambda
        (map car (cadr exp))
        (cddr exp))
       (map cadr (cadr exp)))
      (let* ((vars (map car (caddr exp)))
             (proc (cadr exp))
             (head (cons proc vars))
             (body (cdddr exp)))
        (make-let
         (caddr exp)
         (list
          (make-definition head body)
          head)))))
;4.1.3
(define (false? x)
  (eq? x false))
(define (true? x)
  (not (eq? x false)))
(define (make-procedure parameters body env)
  ;4.16
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p)
  (cadr p))
(define (procedure-body p)
  (caddr p))
(define (procedure-environment p)
  (cadddr p))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             ;4.16a
             (if (eq? '*unassigned* (car vals))
                 (error "Unassigned variable" var)
                 (car vals)))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
;4.12
(define (lookup-var-in-frame var vars vals)
  (cond ((null? vars) false)
        ((eq? var (car vars)) (cons vars vals))
        (else (lookup-var-in-frame var (cdr vars) (cdr vals)))))
(define (set-variable-value!-2 var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (let ((result (lookup-var-in-frame
                       var
                       (frame-variables frame)
                       (frame-values frame))))
          (if result
              (set-car! (cdr result) val)
              (set-variable-value!-2
               var val
               (enclosing-environment env)))))))
(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (result (lookup-var-in-frame var
                                      (frame-variables frame)
                                      (frame-values frame))))
    (if result
        (set-car! (cdr result) val)
        (add-binding-to-frame! var val frame))))
(define (lookup-variable-value-2 var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (let ((result (lookup-var-in-frame
                       var
                       (frame-variables frame)
                       (frame-values frame))))
          (if result
              (cadr result)
              (lookup-variable-value-2
               var
               (enclosing-environment env)))))))
;4.13
(define (make-unbound! var env)
  (let* ((frame (first-frame env))
         (vars (frame-variables frame))
         (vals (frame-values frame)))
    (define (scan pre-vars pre-vals vars vals)
      (if (not (null? vars))
          (if (eq? var (car vars))
              (begin (set-cdr! pre-vars (cdr vars))
                     (set-cdr! pre-vals (cdr vals)))
              (scan vars vals (cdr vars) (cdr vals)))))
    (if (not (null? vars))
        (if (eq? var (car vars))
            (begin (set-car! frame (cdr vars))
                   (set-cdr! frame (cdr vals)))
            (scan vars vals (cdr vars) (cdr vals))))))
;4.1.4
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define the-global-environment
  (setup-environment))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
;4.16
(define (append x y)
    (if (null? x) y (cons (car x) (append (cdr x) y))))
(define (scan-out-defines body)
  (let* ((definitions
           (filter (lambda (x)
                     (and (pair? x) (eq? (car x) 'define))) body))
         (non-definitions
          (filter (lambda (x)
                    (or (not (pair? x))
                        (not (eq? (car x) 'define)))) body))
         (let-vars (map definition-variable definitions))
         (let-vals (map definition-value definitions))
         (let-bindings
          (map (lambda (x) (list x ''*unassigned*)) let-vars))
         (assignments
          (map (lambda (x y) (list 'set! x y)) let-vars let-vals)))
    (if (null? let-bindings)
        body
        (list (make-let let-bindings (append assignments non-definitions))))))
;4.20
(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec->combination exp)
  (let* ((vars (map car (cadr exp)))
         (vals (map cadr (cadr exp)))
         (let-bindings
          (map (lambda (x) (list x ''*unassigned*)) vars))
         (assignments
          (map (lambda (x y) (list 'set! x y)) vars vals)))
    (make-let let-bindings (append assignments (cddr exp)))))
;4.21
(define (f x)
  ((lambda (ev? od?)
     (ev? ev? od? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
