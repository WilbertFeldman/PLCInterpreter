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
	   [var-exp (id)
		    (apply-env env id init-env)]
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

(define init-env
  (extend-env
   *prim-proc-names*
   (list->vector (map prim-proc
	*prim-proc-names*))
   (empty-env)))

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
        (let-exp (list 'x) (list (car bodies)) (list (if-exp (var-exp 'x) (var-exp 'x) (expand-or (cdr bodies))))))))

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


;This is the start.
(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
