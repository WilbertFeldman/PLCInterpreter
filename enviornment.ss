; Environment definitions for CSSE 304 Scheme interpreter.
					; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals l1 l2 ref-env env)
    (extended-env-record syms vals l1 l2 ref-env env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))

(define apply-global-env
  (lambda (env sym)
    (cases environment env ;(cases name-of-datatype switch-val)
	   [extended-env-record (syms vals l1 l2 ref-env env)
				(let ([pos (list-find-position sym syms)])
				  (if (number? pos)
				      (vector-ref vals pos)
				      (eopl:error 'env "variable ~s not found." sym)))]
	   [else (eopl:error 'env "Not extended-env-record")])))


(define apply-env
  (lambda (env sym)
    (cases environment env
	   [empty-env-record ()
			     (apply-global-env init-env sym)]
	   [extended-env-record (syms vals l1 l2 ref-env parent-env)
				(let ([var-pos (list-find-position sym syms)]
              [ref-pos (list-find-position sym l1)])
          (if (number? ref-pos)
            (apply-env ref-env (list-ref l2 ref-pos))
        	  (if (number? var-pos)
        	      (vector-ref vals var-pos)
        	      (apply-env parent-env sym))))])))


(define set-val
  (lambda (env sym val)
    (cases environment env
     [empty-env-record ()
        (define-val sym val init-env)]
     [extended-env-record (syms vals l1 l2 ref-env env)
        (let ([var-pos (list-find-position sym syms)]
              [ref-pos (list-find-position sym l1)])
          (if (number? ref-pos)
            (set-val ref-env (list-ref l2 ref-pos) val))
            (if (number? var-pos)
                (vector-set! vals var-pos val)
                (set-val env sym val)))])))


(define define-val
  (lambda (var val env)
    (cases environment env
      [empty-env-record ()
         (eopl:error 'env "Wrong2")]
      [extended-env-record (syms vals l1 l2 ref-env env)
        (set! init-env (extended-env-record (cons var syms) (list->vector (cons val (vector->list vals))) l1 l2 ref-env env))])))


(define extend-env-recursively
 (lambda (vars vals old-env)
   (let ([len (length vars)])
     (let ([vec (make-vector len)])
       (let ([env (extended-env-record
                   vars vec '() '() (empty-env) old-env)])
         (for-each
            (lambda (pos val)
              (vector-set! vec
                           pos
                           (eval-exp val env)))
            (iota len)
            vals)
          env)))))
