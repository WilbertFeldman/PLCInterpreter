; top-level-eval evaluates a form in the global environment
; A language can either be compiled or interpreted. Scheme is a language that can be interpreted.
;"Interpreted" means that it executes the code immediately without translating human-readable code to computer-readable bits.

;Creates the first empty environment
(define top-level-eval
  (lambda (form)
    (eval-exp-cps form (empty-env) (lambda (x) x))))


(define apply-k
  (lambda (k v)
    (k v)))

(define make-k
  (lambda (v) v))

;Adds variables + values to given assignment. Also does the actual execution of code.
;It checks if 'exp' is a certain type (eg. vars, ifs, etc) and then executes based off information provided.
(define eval-exp-cps
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum)
		    (if (pair? datum)
			     (apply-k k (cadr datum))
			     (apply-k k datum))]
	   [var-exp (id)
		    (apply-k k (apply-env env id init-env))]
	   [app-exp (rator rands)
        (eval-exp-cps rator env (make-k (lambda (proc-val)
                                          (eval-rands-cps rands env (make-k (lambda (args)
                                                                            (apply-proc-cps proc-val args env k)))))))]
     [if-one-exp (condition body)
        (eval-exp-cps condition env (make-k (lambda (eval-exp)
                                                    (if eval-exp
                                                        (eval-exp-cps body env k)))))]
	   [if-exp (condition body1 body2)
         (eval-exp-cps condition env (make-k (lambda (eval-exp)
                                                     (if eval-exp
                                                         (eval-exp-cps body1 env k)
                                                         (eval-exp-cps body2 env k)))))]
	   [lambda-exp (vars bodies)
		       (apply-k k (closure vars bodies env))]
     [while-exp (conds bodies)
      (eval-exp-cps conds env (make-k (lambda (eval-exp)
                                              (if eval-exp
                                                  (eval-bodies-cps bodies env (make-k (lambda (v)
                                                                                  (eval-exp-cps exp env k))))))))]
     [set!-exp (var val)
      (eval-exp-cps val env (make-k (lambda (eval-exp)
                                            (apply-k k (set-val env var eval-exp)))))]
     [define-exp (var body)
     (eval-exp-cps body env (make-k (lambda (eval-exp)
                                            (apply-k k (define-val var eval-exp init-env)))))]
	   [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))


(define eval-bodies-cps
 (lambda (lst env k)
   (cond
    [(null? (cdr lst)) (eval-exp-cps (car lst) env k)]
    [else (eval-exp-cps (car lst) env (make-k (lambda (v)
                                               (eval-bodies-cps (cdr lst) env k))))])))

(define map-cps
  (lambda (proc-cps L k)
    (if (null? L)
      (apply-k k '())
      (if (pair? L)
        (proc-cps (car L)
                  (make-k (lambda (proc)
                                  (map-cps proc-cps (cdr L)
                                                    (make-k (lambda (mapped-cdr)
                                                                    (apply-k k (cons proc mapped-cdr))))))))))))

(define eval-rands-cps
  (lambda (rands env k)
    (map-cps (lambda (x cps-proc) (eval-exp-cps x env cps-proc)) rands k)))


;Applying the actual procedure to elements.
(define apply-proc-cps
  (lambda (proc-value args env k)
    (cases proc-val proc-value
	   [prim-proc (op) (apply-prim-proc-cps op args env k)]
	   [closure (vars bodies env)
		    (let ([new-env (add-lambda-variables-to-enviornment vars args env)])
		      (eval-bodies-cps bodies new-env k))]
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

(define apply-prim-proc-cps
  (lambda (prim-proc args env k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (apply-k k (apply add1 args))]
      [(sub1) (apply-k k (apply sub1 args))]
      [(=) (apply-k k (apply = args))]
      [(<) (apply-k k (apply < args))]
      [(>) (apply-k k (apply > args))]
      [(<=) (apply-k k (apply <= args))]
      [(>=) (apply-k k (apply >= args))]
      [(not) (apply-k k (apply not args))]
      [(zero?) (apply-k k (apply zero? args))]
      [(cons) (apply-k k (apply cons args))]
      [(append) (apply-k k (apply append args))]
      [(car) (apply-k k (apply car args))]
      [(cdr) (apply-k k (apply cdr args))]
      [(list) (apply-k k args)]
      [(null?) (apply-k k (apply null? args))]
      [(assq) (apply-k k (apply assq args))]
      [(eq?) (apply-k k (apply eq? args))]
      [(eqv?) (apply-k k (apply eqv? args))]
      [(equal?) (apply-k k (apply equal? args))]
      [(atom?) (apply-k k (apply atom? args))]
      [(length) (apply-k k (apply length args))]
      [(list->vector) (apply-k k (apply list->vector args))]
      [(list?) (apply-k k (apply list? args))]
      [(list-tail) (apply-k k (apply list-tail args))]
      [(pair?) (apply-k k (apply pair? args))]
      [(procedure?) (apply-k k (apply proc-val? args))]
      [(vector->list) (apply-k k (apply vector->list args))]
      [(vector) (apply-k k (apply vector args))]
      [(make-vector) (apply-k k (apply make-vector args))]
      [(vector-ref) (apply-k k (apply vector-ref args))]
      [(vector?) (apply-k k (apply vector? args))]
      [(number?) (apply-k k (apply number? args))]
      [(symbol?) (apply-k k (apply symbol? args))]
      [(set-car!) (apply-k k (apply set-car! args))]
      [(set-cdr!) (apply-k k (apply set-cdr! args))]
      [(vector-set!) (apply-k k (apply vector-set! args))]
      [(display) (apply-k k (apply display args))]
      [(newline) (apply-k k (apply newline args))]
      [(caar) (apply-k k (apply caar args))]
      [(cadr) (apply-k k (apply cadr args))]
      [(cdar) (apply-k k (apply cdar args))]
      [(cddr) (apply-k k (apply cddr args))]
      [(caaar) (apply-k k (apply caaar args))]
      [(caadr) (apply-k k (apply caadr args))]
      [(cadar) (apply-k k (apply cadar args))]
      [(cdaar) (apply-k k (apply cdaar args))]
      [(caddr) (apply-k k (apply caddr args))]
      [(cdadr) (apply-k k (apply cdadr args))]
      [(cddar) (apply-k k (apply cddar args))]
      [(cdddr) (apply-k k (apply cdddr args))]
      [(map) (our-map-cps (1st args) (2nd args) env k)]
      [(apply) (apply-k k (our-apply-cps (1st args) env (last args) (all-but-last (cdr args)) k))]
      [(quotient) (apply-k k (apply quotient args))]
      [(member) (apply-k k (apply member args))]
      [(product) (apply-k k (apply product args))]
      [(union) (apply-k k (apply union args))]
      [else (error 'apply-prim-proc-cps
		   "Bad primitive procedure name: ~s"
		   prim-op)])))
;
; (define apply-cps
;   (lambda (proc lst k)))

(define union ; s1 and s2 are sets of symbols.
  (lambda (s1 s2)
    (let loop ([s1 s1])
      (cond [(null? s1) s2]
            [(memq (car s1) s2) (loop (cdr s1))]
            [else (cons (car s1) (loop (cdr s1)))]))))

(define our-map-cps
  (lambda (proc ls env k)
    (if (null? ls)
      (apply-k k '())
      (apply-proc-cps proc (car ls) env (make-k (lambda (var)
                                        (our-map-cps proc (cdr ls) env (make-k (lambda (map)
                                                                        (apply-k k (cons var map)))))))))))

(define our-apply-cps
  (lambda (proc env lst objs k)
    (apply-proc-cps proc (append objs lst) env k)))

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
