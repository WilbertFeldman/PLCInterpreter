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
