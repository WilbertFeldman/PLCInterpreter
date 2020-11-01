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
	   [var-exp (depth pos)
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
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
