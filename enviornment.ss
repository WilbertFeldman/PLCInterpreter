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
  (lambda (env sym global-env)
    (cases environment env
	   [empty-env-record ()
			     (apply-global-env global-env sym)]
	   [extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
				  (if (number? pos)
				      (vector-ref vals pos)
				      (apply-env env sym global-env)))])))



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
