					; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    (eval-exp form (empty-env))))

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
		    (let ([new-env (extend-env vars (map (lambda (x) (eval-exp x env)) vals) env)])
		      (eval-bodies bodies new-env))]
	   [lambda-exp (vars bodies)
		       (closure vars bodies env)]
	   [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define eval-bodies
  (lambda (lst env)
    (cond
     [(null? (cdr lst)) (eval-exp (car lst) env)]
     [else (eval-exp (car lst) env) (eval-bodies (cdr lst) env)])))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
	   [prim-proc (op) (apply-prim-proc op args)]
	   [closure (vars bodies env)
		    (let ([new-env (add-lambda-variables-to-enviornment vars args env)])
		      (eval-bodies bodies new-env))]
	   [else (error 'apply-proc
			"Attempt to apply bad procedure: ~s"
			proc-value)])))

;; I want a better name for this
(define (add-lambda-variables-to-enviornment vars args env)
  (cond [(symbol? vars)
	 (extend-env (list vars) (list args) env)]
	[(list? vars)
	 (extend-env vars args env)]
	[else
	 (display "I don't feel like handling this case rn")
	 ]))


(define *prim-proc-names* '(+ - * / add1 sub1 = < > <= >= not zero? cons car cdr cddr
			      list null? assq eq? equal? atom? length list->vector
			      list? pair? procedure? vector->list vector make-vector
			      vector-ref vector? number? symbol? set-car! set-cdr!
			      vector-set! display newline caar cadr cdar cddr caaar
			      caadr cadar cdaar caddr cdadr cddar cdddr map apply))

(define init-env
  (extend-env
   *prim-proc-names*
   (map prim-proc
	*prim-proc-names*)
   (empty-env)))

(define apply-prim-proc
  (lambda (prim-proc args)
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
      [(car) (apply car args)]
      [(cdr) (apply cdr args)]
      [(list) args]
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(pair?) (apply pair? args)]
      [(procedure?) (apply procedure? args)]
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
      [(map) (apply map args)]
      [(apply) (apply apply args)]
      [else (error 'apply-prim-proc
		   "Bad primitive procedure name: ~s"
		   prim-op)])))

(define rep
  (lambda ()
    (display "--> ")
    (let ([answer (top-level-eval (parse-exp (read)))])
      (eopl:pretty-print answer) (newline)
      (rep))))

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))