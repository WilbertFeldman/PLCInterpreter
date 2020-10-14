;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (value literal?)]
  [lambda-exp
   (vars (lambda (x) (or ((list-of symbol?) x) (improper-list-of-symbols? x) (symbol? x))))
   (bodies (list-of expression?))]
  [if-exp
   (condition expression?)
   (body1 expression?)
   (body2 expression?)]
  [let-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [namedlet-exp
   (name var-exp?)
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [set!-exp
   (var var-exp?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))])


					;type helpers
(define var-exp?
  (lambda (x)
    (if (expression? x)
	(cases expression x
	       [var-exp (id) #t]
	       [else #f])
	#f)))

(define (improper-list-of-symbols? list)
  (if (pair? list)
      (and (symbol? (1st list)) (improper-list-of-symbols? (cdr list)))
      (symbol? list)))

(define (literal? x)
  (and (not (symbol? x)) (or (not (pair? x)) (equal? 'quote (car x)))))

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

					; datatype for procedures.  At first there is only one
					; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (args (lambda (x) (or ((list-of symbol?) x) (improper-list-of-symbols? x) (symbol? x))))
   (bodies (list-of expression?))
   (env environment?)])
