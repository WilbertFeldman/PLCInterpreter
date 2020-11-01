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
       [(equal? (1st datum) 'ref)
         (parse-ref datum)]
       [else
	(parse-app datum)])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (valid-vars vars)
  (cond [(list? vars)
	 (andmap var? vars)]
	[(pair? vars)
	 (improper-list-of-vars? vars)]
	[else
	 (symbol? vars)]))
					;4 helpers
(define (parse-lambda datum)
  (cond [(not (valid-vars (2nd datum)))
	 (eopl:error 'parse-exp "invalid variables for lambda: ~s")]
	[(> 3 (length datum))
	 (eopl:error 'parse-exp "improperly formated lambda expected >2 parts: ~s" datum)]
	[else
	 (cond
	  [(symbol? (2nd datum))
	   (lambda-exp (parse-exp (2nd datum))
		       (map parse-exp (cddr datum)))]
	  [(not (list? (2nd datum)))
	   (lambda-exp (improper-list-map parse-exp (2nd datum))
		       (map parse-exp (cddr datum)))]
	  [else
	   (lambda-exp (map parse-exp (2nd datum))
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

(define (parse-ref datum)
  (ref-exp (2nd datum)))


(define (improper-list-map proc list)
  (if (pair? x)
      (cons (proc (car list)) (improper-list-map proc (cdr list)))
      (proc x)))

(define (improper-list-of-vars vars)
  (if (pair? vars)
    (and (var? (car vars)) (improper-list-of-vars (cdr vars)))
    (var? vars)))

(define (var? x)
  (or (and (list? x) (equal? 'ref (car x)) (symbol? (cadr x))) (symbol? x)))

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
