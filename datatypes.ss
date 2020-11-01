;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

;Defined all the different datatypes so that the parser can format what it sees into a type specified here.
(define-datatype expression expression? ;Expression is a datatype. Expression? is a predicate that checks if the obj is an expression.
  [var-exp
   (id symbol?)]
  [lit-exp
   (value literal?)]
  [lambda-exp
   (vars (lambda (x) (or ((list-of expression?) x) (improper-list-of-expressions? x) (expression? x))))
   (bodies (list-of expression?))]
  [if-one-exp
   (condtion expression?)
   (body1 expression?)]
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
   (name symbol?)
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [set!-exp
   (var symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))]
  [case-exp
    (exps expression?)
    (vals (list-of (lambda (x) (or (expression? x) ((list-of expression?) x)))))
    (bodies (list-of (list-of expression?)))]
  [and-exp
    (bodies (list-of expression?))]
  [or-exp
    (bodies (list-of expression?))]
  [begin-exp
    (bodies (list-of expression?))]
  [cond-exp
    (conds (list-of expression?))
    (bodies (list-of (list-of expression?)))]
  [else-exp]
  [while-exp
    (conds expression?)
    (bodies (list-of expression?))]
  [define-exp
    (val symbol?)
    (body expression?)]
  [ref-exp
    (var symbol?)])


					;type helpers
(define var-exp?
  (lambda (x)
    (if (expression? x)
	(cases expression x
	       [var-exp (id) #t]
	       [else #f])
	#f)))

(define (improper-list-of-expressions? list)
  (if (pair? list)
      (and (expression? (1st list)) (improper-list-of-expressions? (cdr list)))
      (expression? list)))

(define (literal? x)
  (and (not (symbol? x)) (or (not (pair? x)) (equal? 'quote (car x)))))

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals vector?)
   (new-env-vars (list-of symbol?))
   (old-env-vars (list-of symbol?))
   (ref-env environment?)
   (env environment?)))

					; datatype for procedures.  At first there is only one
					; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (args (lambda (x) (or ((list-of expression?) x) (improper-list-of-expressions? x) (expression? x))))
   (bodies (list-of expression?))
   (env environment?)])


; Environment definitions for CSSE 304 Scheme interpreter.
