;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

;Defined all the different datatypes so that the parser can format what it sees into a type specified here.
(define-datatype expression expression? ;Expression is a datatype. Expression? is a predicate that checks if the obj is an expression.
  [address-exp
   (depth number?)
   (pos (lambda (x) (or (number? x) (symbol? x))))]
  [var-exp
    (id symbol?)]
  [lit-exp
   (value literal?)]
     [lambda-exp
      (vars (lambda (x) (or ((list-of symbol?) x) (improper-list-of-symbols? x) (symbol? x))))
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
    (body expression?)])


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
   (vals vector?)
   (env environment?)))

					; datatype for procedures.  At first there is only one
					; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (args (lambda (x) #t))
   (bodies (list-of expression?))
   (env environment?)])


; Environment definitions for CSSE 304 Scheme interpreter.
; Environment definitions for CSSE 304 Scheme interpreter.
					; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))

(define apply-global-env
  (lambda (env sym)
    (cases environment env ;(cases name-of-datatype switch-val)
	   [extended-env-record (syms vals env)
				(let ([pos (list-find-position sym syms)])
				  (if (number? pos)
				      (vector-ref vals pos)
				      (eopl:error 'env "variable ~s not found." sym)))]
	   [else (eopl:error 'env "Not extended-env-record")])))


(define apply-env
  (lambda (env depth pos global-env)
    (cond
      [(= depth -1) (apply-global-env global-env pos)]
      [(= depth 0)
        (cases environment env
          [extended-env-record (syms vals env)
            (vector-ref vals pos)]
          [else
            (eopl:error 'apply-env "Not extended-env-record")])]
      [else
        (cases environment env
          [extended-env-record (syms vals env)
            (apply-env env (sub1 depth) pos global-env)]
          [else
            (eopl:error 'apply-env "Not extended-env-record")])])))


(define set-val
  (lambda (env sym val)
    (cases environment env
     [empty-env-record ()
        (define-val sym val init-env)]
     [extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (vector-set! vals pos val)
              (set-val env sym val)))])))


(define define-val
  (lambda (var val env)
    (cases environment env
      [empty-env-record ()
         (eopl:error 'env "Wrong2")]
      [extended-env-record (syms vals env)
        (set! init-env (extended-env-record (cons var syms) (list->vector (cons val (vector->list vals))) (empty-env)))])))


(define extend-env-recursively
 (lambda (vars vals old-env)
   (let ([len (length vars)])
     (let ([vec (make-vector len)])
       (let ([env (extended-env-record
                   vars vec old-env)])
         (for-each
            (lambda (pos val)
              (vector-set! vec
                           pos
                           (eval-exp val env)))
            (iota len)
            vals)
          env)))))
;Transforming more complex procedures to "core forms" (a.k.a. things you NEED in Scheme)
;So that you can deal with less cases in eval-exp
(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [lambda-exp (vals bodies)
        (lambda-exp vals (map syntax-expand bodies))]
      [if-one-exp (condition body)
        (if-one-exp (syntax-expand condition)
          (syntax-expand body))]
 	    [if-exp (condition body1 body2)
 		   (if-exp (syntax-expand condition)
 		       (syntax-expand body1)
 		       (syntax-expand body2))]
      [let-exp (vars vals bodies)
        (app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand vals))]
      [let*-exp (vars vals bodies)
        (syntax-expand (expand-let* vars vals bodies))]
      [letrec-exp (vars vals bodies)
        (letrec-exp vars (map syntax-expand vals) (map syntax-expand bodies))]
      [namedlet-exp (name vars vals bodies)
        (syntax-expand (expand-named-let name vars vals bodies))]
      [app-exp (rator rands)
        (app-exp (syntax-expand rator) (map syntax-expand rands))]
      [while-exp (conds bodies)
        (while-exp (syntax-expand conds) (map syntax-expand bodies))]
      [cond-exp (conds bodies)
        (syntax-expand (expand-cond (map syntax-expand conds) (map (lambda (x) (map syntax-expand x)) bodies)))]
      [case-exp (exps vals bodies)
        (syntax-expand (expand-case (syntax-expand exps)
          (map (lambda (x)
            (if (expression? x)
              (syntax-expand x)
              (map syntax-expand x))) vals)
          (map (lambda (x) (map syntax-expand x)) bodies)))]
      [and-exp (bodies)
        (syntax-expand (expand-and (map syntax-expand bodies)))]
      [or-exp (bodies)
        (syntax-expand (expand-or (map syntax-expand bodies)))]
      [begin-exp (bodies)
        (app-exp (lambda-exp '() (map syntax-expand bodies)) '())]
      [set!-exp (var body)
        (set!-exp var (syntax-expand body))]
      [define-exp (var body)
        (define-exp var (syntax-expand body))]
      [else
        exp])))

;Helpers for syntax expand

(define (expand-named-let name vars vals bodies)
  (app-exp (letrec-exp (list name) (list (lambda-exp vars bodies)) (list (var-exp name))) vals))

(define (expand-and bodies)
  (if (null? bodies)
    (lit-exp '#t)
      (if (null? (cdr bodies))
        (car bodies)
        (let-exp (list 'x) (list (car bodies)) (list (if-exp (var-exp 'x) (expand-and (cdr bodies)) (lit-exp '#f)))))))

(define (expand-or bodies)
  (if (null? bodies)
    (lit-exp '#f)
      (if (null? (cdr bodies))
        (car bodies)
        (let-exp (list '_x) (list (car bodies)) (list (if-exp (var-exp '_x) (var-exp '_x) (expand-or (cdr bodies))))))))

(define (expand-let* vars vals bodies)
  (if (null? (cdr vars))
    (let-exp (list (car vars)) (list (car vals)) bodies)
    (let-exp (list (car vars)) (list (car vals)) (list (expand-let* (cdr vars) (cdr vals) bodies)))))

(define (expand-cond conds bodies)
  (if (null? (cdr bodies))
    (cases expression (car conds)
      [else-exp ()
        (if-one-exp (lit-exp '#t) (begin-exp (car bodies)))]
      [else
        (if-one-exp (car conds) (begin-exp (car bodies)))])
    (if-exp (car conds) (begin-exp (car bodies)) (expand-cond (cdr conds) (cdr bodies)))))

(define (expand-case exps vals bodies)
  (if (null? (cdr bodies))
    (if (expression? (car vals))
      (cases expression (car vals)
        [else-exp ()
          (if-one-exp (lit-exp '#t) (begin-exp (car bodies)))]
        [else
          (if-one-exp (app-exp (var-exp 'equal?) (list exps (car vals))) (begin-exp (car bodies)))])
      (if-one-exp (app-exp (var-exp 'member) (list exps (app-exp (var-exp 'list) (car vals)))) (begin-exp (car bodies))))
    (if (expression? (car vals))
      (cases expression (car vals)
        [else-exp ()
          (if-exp (lit-exp '#t) (begin-exp (car bodies)) (expand-case exps (cdr vals) (cdr bodies)))]
        [else
          (if-exp (app-exp (var-exp 'equal?) (list exps (car vals))) (begin-exp (car bodies)) (expand-case exps (cdr vals) (cdr bodies)))])
      (if-exp (app-exp (var-exp 'member) (list exps (app-exp (var-exp 'list) (car vals)))) (begin-exp (car bodies)) (expand-case exps (cdr vals) (cdr bodies))))))
; top-level-eval evaluates a form in the global environment
; A language can either be compiled or interpreted. Scheme is a language that can be interpreted.
;"Interpreted" means that it executes the code immediately without translating human-readable code to computer-readable bits.

;Creates the first empty environment
(define top-level-eval
  (lambda (form)
    (eval-exp form (empty-env))))

;Adds variables + values to given assignment. Also does the actual execution of code.
;It checks if 'exp' is a certain type (eg. vars, ifs, etc) and then executes based off information provided.
(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum)
		    (if (pair? datum)
			     (cadr datum)
			      datum)]
	   [address-exp (depth pos)
		    (apply-env env depth pos init-env)]
	   [app-exp (rator rands)
		    (let ([proc-value (eval-exp rator env)]
			  [args (eval-rands rands env)])
		      (apply-proc proc-value args env))]
     [if-one-exp (condition body)
        (if (eval-exp condition env)
            (eval-exp body env))]
	   [if-exp (condition body1 body2)
		   (if (eval-exp condition env)
		       (eval-exp body1 env)
		       (eval-exp body2 env))]
	   [let-exp (vars vals bodies)
		    (let ([new-env (extend-env vars (list->vector (map (lambda (x) (eval-exp x env)) vals)) env)])
		      (eval-bodies bodies new-env))]
     [letrec-exp (vars vals bodies)
     (eval-bodies bodies (extend-env-recursively vars vals env))]
	   [lambda-exp (vars bodies)
		       (closure vars bodies env)]
     [while-exp (conds bodies)
      (if (eval-exp conds env)
        (begin (eval-bodies bodies env) (eval-exp exp env)))]
     [set!-exp (var val)
      (set-val env var (eval-exp val env))]
     [define-exp (var body)
      (define-val var (eval-exp body env) init-env)]
	   [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))



(define eval-bodies
  (lambda (lst env)
    (cond
     [(null? (cdr lst)) (eval-exp (car lst) env)]
     [else (eval-exp (car lst) env) (eval-bodies (cdr lst) env)])))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))


;Applying the actual procedure to elements.
(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
	   [prim-proc (op) (apply-prim-proc op args env)]
	   [closure (vars bodies env)
		    (let ([new-env (add-lambda-variables-to-enviornment vars args env)])
		      (eval-bodies bodies new-env))]
	   [else (error 'apply-proc
			"Attempt to apply bad procedure: ~s"
			proc-value)])))

;; I want a better name for this
(define (add-lambda-variables-to-enviornment vars args env)
  (cond
    [(symbol? vars)
	   (extend-env (list vars) (list->vector (list args)) env)]
	  [(list? vars)
	   (extend-env vars (list->vector args) env)]
	  [else
      (let ([list-of-vars (list-of-unknown-vars vars)])
        (extend-env list-of-vars (list->vector (list-of-unknown-args args (length list-of-vars))) env))]))

(define list-of-unknown-vars
  (lambda (vars)
    (cond
      [(not (pair? vars)) (list vars)]
      [else (cons (car vars) (list-of-unknown-vars (cdr vars)))])))

(define list-of-unknown-args
  (lambda (args len)
    (cond
      [(= len 1) (list args)]
      [else (cons (car args) (list-of-unknown-args (cdr args) (- len 1)))])))


(define *prim-proc-names* '(+ - * / add1 sub1 = < > <= >= not zero? cons append car cdr cddr
			      list null? assq eq? eqv? equal? atom? length list->vector
			      list? pair? procedure? vector->list vector make-vector
			      vector-ref vector? number? symbol? set-car! set-cdr!
			      vector-set! display newline caar cadr cdar cddr caaar
			      caadr cadar cdaar caddr cdadr cddar cdddr map apply quotient member
            list-tail product))


(define make-init-env
  (lambda ()
   (extend-env
    *prim-proc-names*
    (list->vector (map prim-proc
   *prim-proc-names*))
    (empty-env))))

(define init-env
  (make-init-env))

(define reset-global-env
 (lambda () (set! init-env (make-init-env))))

(define apply-prim-proc
  (lambda (prim-proc args env)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (apply add1 args)]
      [(sub1) (apply sub1 args)]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(not) (apply not args)]
      [(zero?) (apply zero? args)]
      [(cons) (apply cons args)]
      [(append) (apply append args)]
      [(car) (apply car args)]
      [(cdr) (apply cdr args)]
      [(list) args]
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
      [(eqv?) (apply eqv? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(list-tail) (apply list-tail args)]
      [(pair?) (apply pair? args)]
      [(procedure?) (apply proc-val? args)]
      [(vector->list) (apply vector->list args)]
      [(vector) (apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (apply vector-ref args)]
      [(vector?) (apply vector? args)]
      [(number?) (apply number? args)]
      [(symbol?) (apply symbol? args)]
      [(set-car!) (apply set-car! args)]
      [(set-cdr!) (apply set-cdr! args)]
      [(vector-set!) (apply vector-set! args)]
      [(display) (apply display args)]
      [(newline) (apply newline args)]
      [(caar) (apply caar args)]
      [(cadr) (apply cadr args)]
      [(cdar) (apply cdar args)]
      [(cddr) (apply cddr args)]
      [(caaar) (apply caaar args)]
      [(caadr) (apply caadr args)]
      [(cadar) (apply cadar args)]
      [(cdaar) (apply cdaar args)]
      [(caddr) (apply caddr args)]
      [(cdadr) (apply cdadr args)]
      [(cddar) (apply cddar args)]
      [(cdddr) (apply cdddr args)]
      [(map) (our-map (1st args) env (2nd args) (cddr args))]
      [(apply) (our-apply (1st args) env (last args) (all-but-last (cdr args)))]
      [(quotient) (apply quotient args)]
      [(member) (apply member args)]
      [(product) (apply product args)]
      [(union) (apply union args)]
      [else (error 'apply-prim-proc
		   "Bad primitive procedure name: ~s"
		   prim-op)])))

(define union ; s1 and s2 are sets of symbols.
  (lambda (s1 s2)
    (let loop ([s1 s1])
      (cond [(null? s1) s2]
            [(memq (car s1) s2) (loop (cdr s1))]
            [else (cons (car s1) (loop (cdr s1)))]))))
(define our-map
  (lambda (proc env lst lsts)
    (if (null? lst)  '()
          (cons (apply-proc proc (cons (car lst) (map car lsts)) env)
            (our-map proc env (cdr lst) (map cdr lsts))))))

(define our-apply
  (lambda (proc env lst objs)
    (apply-proc proc (append objs lst) env)))

(define last
  (lambda (lst)
    (car (reverse lst))))

(define all-but-last
  (lambda (lst)
    (reverse (cdr (reverse lst)))))

(define rep
  (lambda ()
    (display "--> ")
    (let ([answer (top-level-eval (parse-exp (read)))])
      (eopl:pretty-print answer) (newline)
      (rep))))

;This is the start.
(define eval-one-exp
  (lambda (x) (top-level-eval (lexical-address (syntax-expand (parse-exp x))))))
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

;The parser activates first. This makes human-readable code more familiar to the computer, making it easier to evaluate.
;Eval-exp is easier to write thanks to parse-exp.
(define parse-exp
  (lambda (datum)
    (cond
     [(symbol? datum)
      (var-exp datum)]
     [(literal? datum)
      (lit-exp datum)]
     [(list? datum)
      (cond
       [(eqv? (1st datum) 'lambda)
	             (parse-lambda datum)]
       [(equal? (1st datum) 'if)
	            (parse-if datum)]
       [(and (equal? (1st datum) 'let)
            	(and (not (null? (cdr datum))) (symbol? (2nd datum))))
            	(parse-named-let datum)]
       [(equal? (1st datum) 'let)
        (parse-let let-exp datum)]
       [(equal? (1st datum) 'let*)
      	(parse-let let*-exp datum)]
       [(equal? (1st datum) 'letrec)
	      (parse-let letrec-exp datum)]
       [(equal? (1st datum) 'set!)
	      (parse-set! datum)]
       [(equal? (1st datum) 'case)
        (parse-case datum)]
       [(equal? (1st datum) 'and)
        (parse-and datum)]
       [(equal? (1st datum) 'or)
        (parse-or datum)]
       [(equal? (1st datum) 'begin)
        (parse-begin datum)]
       [(equal? (1st datum) 'cond)
        (parse-cond datum)]
       [(equal? (1st datum) 'while)
        (parse-while datum)]
       [(equal? (1st datum) 'for)
         (parse-for datum)]
       [(equal? (1st datum) 'define)
         (parse-define datum)]
       [else
	(parse-app datum)])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (valid-vars vars)
  (cond [(list? vars)
	 (andmap symbol? vars)]
	[(pair? vars)
	 (improper-list-of-symbols? vars)]
	[else
	 (symbol? vars)]))
					;4 helpers
(define (parse-lambda datum)
  (cond [(not (valid-vars (2nd datum)))
	 (eopl:error 'parse-exp "invalid variables for lambda: ~s" datum)]
	[(> 3 (length datum))
	 (eopl:error 'parse-exp "improperly formated lambda expected >2 parts: ~s" datum)]
	[else
	 (cond
	  [(symbol? (2nd datum))
	   (lambda-exp (2nd datum)
		       (map parse-exp (cddr datum)))]
	  [(not (list? (2nd datum)))
	   (lambda-exp (2nd datum)
		       (map parse-exp (cddr datum)))]
	  [else
	   (lambda-exp (2nd datum)
		       (map parse-exp (cddr datum)))])]))

(define (parse-if datum)
  (cond
   [ (or (< (length datum) 3) (> (length datum) 4))
    (eopl:error 'parse-exp "if requires at least 3 parts: cond, body1. received: ~s" datum)]
   [(= (length datum) 3)
    (if-one-exp (parse-exp (2nd datum))
                (parse-exp (3rd datum)))]
   [else
    (if-exp (parse-exp (2nd datum))
	    (parse-exp (3rd datum))
	    (parse-exp (cadddr datum)))]))

(define (parse-let type datum)
  (cond
   [(> 3 (length datum))
    (eopl:error 'parse-exp "length is less than 3")]
   [(not (and
	  (list? (2nd datum))
	  (andmap (lambda (x)
		    (and (list? x) (= 2 (length x)) (symbol? (1st x))))
		  (2nd datum))))
    (eopl:error 'parse-exp "first elements must be symbols")]
   [else
    (type
     (map 1st (2nd datum))
     (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
     (map parse-exp (cddr datum)))]))

(define (parse-named-let datum)
  (cond
   [(> 3 (length datum)) (eopl:error 'parse-exp "length is less than 4")]
   [(and (list? (2nd datum))
	 (andmap (lambda (x)
		   (and (list? x) (= 2 (length x)) (symbol? (1st x))))
		 (2nd datum)))
    (eopl:error 'parse-exp "first elements must be symbols")]
   [(not (symbol? (2nd datum))) (eopl:error 'parse-exp "let must be named")]
   [else
    (namedlet-exp (2nd datum)
		  (map 1st (3rd datum))
		  (map (lambda (x) (parse-exp (2nd x))) (3rd datum))
		  (map parse-exp (cdddr datum)))]))

(define (parse-set! datum)
  (cond
   [(not (= (length datum) 3))
    (eopl:error 'parse-exp "length is not equal to 3")]
   [(not (symbol? (2nd datum)))
    (eopl:error 'parse-exp "the first element must be a symbol")]
   [else
    (set!-exp (2nd datum) (parse-exp (3rd datum)))]))

(define (parse-app datum)
  (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum))))

(define (parse-case datum)
  (case-exp (parse-exp (2nd datum)) (parse-cases (cddr datum)) (map (lambda (x) (map parse-exp x)) (map cdr (cddr datum)))))

(define (parse-and datum)
  (and-exp (map parse-exp (cdr datum))))

(define (parse-or datum)
  (or-exp (map parse-exp (cdr datum))))

(define (parse-begin datum)
  (begin-exp (map parse-exp (cdr datum))))

(define (parse-cond datum)
  (cond-exp (parse-conds (cdr datum)) (map (lambda (x) (map parse-exp x)) (map cdr (cdr datum)))))

(define (parse-while datum)
  (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum))))

(define (parse-conds datum)
  (cond
    [(null? datum) '()]
    [else
      (if (equal? (caar datum) 'else)
        (if (null? (cdr datum))
          (list (else-exp))
          (eopl:error 'parse-exp "Conditions after else"))
        (cons (parse-exp (caar datum)) (parse-conds (cdr datum))))]))

(define (parse-cases datum)
  (cond
    [(null? datum) '()]
    [else
      (if (equal? (caar datum) 'else)
        (if (null? (cdr datum))
          (list (else-exp))
          (eopl:error 'parse-exp "Conditions after else"))
        (if (list? (caar datum))
          (cons (map parse-exp (caar datum)) (parse-cases (cdr datum)))
          (cons (parse-exp (caar datum)) (parse-cases (cdr datum)))))]))

(define (parse-define datum)
  (define-exp (2nd datum) (parse-exp (3rd datum))))


(define (improper-list-map proc list)
  (if (pair? x)
      (cons (proc (car list)) (improper-list-map proc (cdr list)))
      (proc x)))


(define (lexical-address lst)
   (let lexical-address ([exp lst] [stk '()])
     (cases expression exp
        [var-exp (var)
         (let ([adr (position var stk)])
          (if (symbol? (cadr adr))
              (address-exp -1 (caddr adr))
              (address-exp (cadr adr) (caddr adr))))]
        [app-exp (rator rands)
          (app-exp (lexical-address rator stk) (map (lambda (x) (lexical-address x stk)) rands))]
        [lambda-exp (vars bodies)
          (cond
            [(symbol? vars)
              (lambda-exp vars (map (lambda (x) (lexical-address x (cons (list vars) stk))) bodies))]
            [(list? vars)
              (lambda-exp vars (map (lambda (x) (lexical-address x (cons vars stk))) bodies))]
            [(not (list? vars))
              (lambda-exp vars (map (lambda (x) (lexical-address x (cons (improper->proper vars) stk))) bodies))])]
        [letrec-exp (vars vals bodies)
           (letrec-exp vars
                       (map (lambda (exp) (lexical-address exp (cons vars stk))) vals)
                       (map (lambda (x) (lexical-address x (cons vars stk))) bodies))]
        [set!-exp (var val)
         (set!-exp var (lexical-address val stk))]
        [if-exp (test body1 body2)
         (if-exp (lexical-address test stk)
                 (lexical-address body1 stk)
                 (lexical-address body2 stk))]
        [if-one-exp (test body1)
         (if-one-exp (lexical-address test stk)
                 (lexical-address body1 stk))]
        [define-exp (var exp)
          (define-exp var (lexical-address exp stk))]
        [else
         exp])))


(define (position var stk)
  (let position ([var var] [stk stk] [depth 0] [lex-position 0])
   (cond
    [(equal? '() stk)
     (list ': 'free var)]
    [(equal? '() (car stk))
     (position var (cdr stk) (add1 depth) 0)]
    [(equal? var (caar stk))
     (list ': depth lex-position)]
    [else
     (position var (cons (cdar stk) (cdr stk)) depth (add1 lex-position))])))

(define (improper->proper lst)
  (cond
    [(pair? lst) (cons (car lst) (improper->proper (cdr lst)))]
    [else (list lst)]))


(define unparse-exp
  (lambda (exp)
    (cases expression exp
	   [var-exp (exp) exp]
	   [lit-exp (exp) exp]
	   [lambda-exp (vars bodies)
		       (cond
			[(symbol? vars)
			 (append (list 'lambda vars)
				 (map unparse-exp bodies))]
			[(list? vars)
			 (append (list 'lambda vars)
				 (map unparse-exp bodies))]
			[(pair? vars)
			 (append (list 'lambda vars)
				 (map unparse-exp bodies))])]
	   [if-exp (conds body1 body2)
		   (list 'if
			 (unparse-exp conds)
			 (unparse-exp body1)
			 (unparse-exp body2))]
	   [let-exp (vars vals bodies)
		    (append (list 'let
				  (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			    (map unparse-exp bodies))]
	   [let*-exp (vars vals bodies)
		     (append (list 'let*
				   (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			     (map unparse-exp bodies))]
	   [letrec-exp (vars vals bodies)
		       (append (list 'letrec
				     (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			       (map unparse-exp bodies))]
	   [namedlet-exp (name vars vals bodies)
			 (list 'let
			       (unparse-exp name)
				     (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			       (map unparse-exp bodies)]
	   [set!-exp (var exp)
		     (list 'set! (unparse-exp var) (unparse-exp exp))]
	   [app-exp (rator rands)
		    (cons (unparse-exp rator) (map unparse-exp rands))])))

    (define (improper-list-map proc list)
      (if (pair? x)
	  (cons (proc (car list)) (improper-list-map proc (cdr list)))
	  (proc x)))
