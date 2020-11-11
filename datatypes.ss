;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

;Defined all the different datatypes so that the parser can format what it sees into a type specified here.
(begin
  (define ignored
    (define-datatype:datatype-checker&registry-updater
      'expression
      '((var-exp (id symbol?)) (lit-exp (value literal?))
         (lambda-exp
           (vars
             (lambda (x)
               (or ((list-of symbol?) x)
                   (improper-list-of-symbols? x)
                   (symbol? x))))
           (bodies (list-of expression?)))
         (if-one-exp (condtion expression?) (body1 expression?))
         (if-exp
           (condition expression?)
           (body1 expression?)
           (body2 expression?))
         (let-exp
           (vars (list-of symbol?))
           (vals (list-of expression?))
           (bodies (list-of expression?)))
         (let*-exp
           (vars (list-of symbol?))
           (vals (list-of expression?))
           (bodies (list-of expression?)))
         (letrec-exp
           (vars (list-of symbol?))
           (vals (list-of expression?))
           (bodies (list-of expression?)))
         (namedlet-exp
           (name symbol?)
           (vars (list-of symbol?))
           (vals (list-of expression?))
           (bodies (list-of expression?)))
         (set!-exp (var symbol?) (body expression?))
         (app-exp (rator expression?) (rands (list-of expression?)))
         (case-exp
           (exps expression?)
           (vals
             (list-of
               (lambda (x)
                 (or (expression? x) ((list-of expression?) x)))))
           (bodies (list-of (list-of expression?))))
         (and-exp (bodies (list-of expression?)))
         (or-exp (bodies (list-of expression?)))
         (begin-exp (bodies (list-of expression?)))
         (cond-exp
           (conds (list-of expression?))
           (bodies (list-of (list-of expression?))))
         (else-exp)
         (while-exp
           (conds expression?)
           (bodies (list-of expression?)))
         (define-exp (val symbol?) (body expression?)))))
  (define expression
    (cons
      '(var-exp lit-exp lambda-exp if-one-exp if-exp let-exp let*-exp
         letrec-exp namedlet-exp set!-exp app-exp case-exp and-exp
         or-exp begin-exp cond-exp else-exp while-exp define-exp)
      '((var-exp id) (lit-exp value) (lambda-exp vars bodies)
         (if-one-exp condtion body1) (if-exp condition body1 body2)
         (let-exp vars vals bodies) (let*-exp vars vals bodies)
         (letrec-exp vars vals bodies)
         (namedlet-exp name vars vals bodies) (set!-exp var body)
         (app-exp rator rands) (case-exp exps vals bodies)
         (and-exp bodies) (or-exp bodies) (begin-exp bodies)
         (cond-exp conds bodies) (else-exp) (while-exp conds bodies)
         (define-exp val body))))
  (define expression?
    (if (symbol? 'expression)
        (lambda args
          (if (null? args)
              (define-datatype:report-error
                'expression?
                "expects 1 argument, not 0.")
              (if (null? (cdr args))
                  (let ([variant (car args)])
                    (let ([type-info expression])
                      (if (and (pair? type-info) (list? (car type-info)))
                          (and (pair? variant)
                               (memq (car variant) (car type-info))
                               #t)
                          (define-datatype:report-error
                            'expression?
                            (string-append
                              "did not get a data type bound to an "
                              "  appropriate structure: ~s. "
                              "  This tends to happen when the type name is "
                              "  bound to a lexical variable.")
                            'type-name
                            type-info))))
                  (define-datatype:report-error
                    'expression?
                    (string-append
                      "expects 1 argument, not ~s. "
                      "  With argument list = ~s.")
                    (length args)
                    args))))
        (define-datatype:report-error
          'expression
          "Type name is not a symbol: ~s."
          'type-name)))
  (define var-exp
    (let ([expected-length (length '(id))]
          [field-names '(id)]
          [pred-names '(symbol?)]
          [preds (list (lambda (x) (symbol? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'var-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(id) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'var-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'var-exp args))))
  (define lit-exp
    (let ([expected-length (length '(value))]
          [field-names '(value)]
          [pred-names '(literal?)]
          [preds (list (lambda (x) (literal? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'lit-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(value) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'lit-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'lit-exp args))))
  (define lambda-exp
    (let ([expected-length (length '(vars bodies))]
          [field-names '(vars bodies)]
          [pred-names '((lambda (x)
                          (or ((list-of symbol?) x)
                              (improper-list-of-symbols? x)
                              (symbol? x)))
                         (list-of expression?))]
          [preds (list
                   (lambda (x)
                     ((lambda (x)
                        (or ((list-of symbol?) x)
                            (improper-list-of-symbols? x)
                            (symbol? x)))
                       x))
                   (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'lambda-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(vars bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'lambda-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'lambda-exp args))))
  (define if-one-exp
    (let ([expected-length (length '(condtion body1))]
          [field-names '(condtion body1)]
          [pred-names '(expression? expression?)]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x) (expression? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'if-one-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(condtion body1) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'if-one-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'if-one-exp args))))
  (define if-exp
    (let ([expected-length (length '(condition body1 body2))]
          [field-names '(condition body1 body2)]
          [pred-names '(expression? expression? expression?)]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x) (expression? x))
                   (lambda (x) (expression? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'if-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(condition body1 body2)
              args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'if-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'if-exp args))))
  (define let-exp
    (let ([expected-length (length '(vars vals bodies))]
          [field-names '(vars vals bodies)]
          [pred-names '((list-of symbol?)
                         (list-of expression?)
                         (list-of expression?))]
          [preds (list
                   (lambda (x) ((list-of symbol?) x))
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'let-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(vars vals bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'let-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'let-exp args))))
  (define let*-exp
    (let ([expected-length (length '(vars vals bodies))]
          [field-names '(vars vals bodies)]
          [pred-names '((list-of symbol?)
                         (list-of expression?)
                         (list-of expression?))]
          [preds (list
                   (lambda (x) ((list-of symbol?) x))
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'let*-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(vars vals bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'let*-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'let*-exp args))))
  (define letrec-exp
    (let ([expected-length (length '(vars vals bodies))]
          [field-names '(vars vals bodies)]
          [pred-names '((list-of symbol?)
                         (list-of expression?)
                         (list-of expression?))]
          [preds (list
                   (lambda (x) ((list-of symbol?) x))
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'letrec-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(vars vals bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'letrec-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'letrec-exp args))))
  (define namedlet-exp
    (let ([expected-length (length '(name vars vals bodies))]
          [field-names '(name vars vals bodies)]
          [pred-names '(symbol?
                         (list-of symbol?)
                         (list-of expression?)
                         (list-of expression?))]
          [preds (list
                   (lambda (x) (symbol? x))
                   (lambda (x) ((list-of symbol?) x))
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'namedlet-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(name vars vals bodies)
              args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'namedlet-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'namedlet-exp args))))
  (define set!-exp
    (let ([expected-length (length '(var body))]
          [field-names '(var body)]
          [pred-names '(symbol? expression?)]
          [preds (list
                   (lambda (x) (symbol? x))
                   (lambda (x) (expression? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'set!-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(var body) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'set!-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'set!-exp args))))
  (define app-exp
    (let ([expected-length (length '(rator rands))]
          [field-names '(rator rands)]
          [pred-names '(expression? (list-of expression?))]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'app-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(rator rands) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'app-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'app-exp args))))
  (define case-exp
    (let ([expected-length (length '(exps vals bodies))]
          [field-names '(exps vals bodies)]
          [pred-names '(expression?
                         (list-of
                           (lambda (x)
                             (or (expression? x)
                                 ((list-of expression?) x))))
                         (list-of (list-of expression?)))]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x)
                     ((list-of
                        (lambda (x)
                          (or (expression? x) ((list-of expression?) x))))
                       x))
                   (lambda (x) ((list-of (list-of expression?)) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'case-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(exps vals bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'case-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'case-exp args))))
  (define and-exp
    (let ([expected-length (length '(bodies))]
          [field-names '(bodies)]
          [pred-names '((list-of expression?))]
          [preds (list (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'and-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'and-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'and-exp args))))
  (define or-exp
    (let ([expected-length (length '(bodies))]
          [field-names '(bodies)]
          [pred-names '((list-of expression?))]
          [preds (list (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'or-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'or-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'or-exp args))))
  (define begin-exp
    (let ([expected-length (length '(bodies))]
          [field-names '(bodies)]
          [pred-names '((list-of expression?))]
          [preds (list (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'begin-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'begin-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'begin-exp args))))
  (define cond-exp
    (let ([expected-length (length '(conds bodies))]
          [field-names '(conds bodies)]
          [pred-names '((list-of expression?)
                         (list-of (list-of expression?)))]
          [preds (list
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) ((list-of (list-of expression?)) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'cond-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(conds bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'cond-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'cond-exp args))))
  (define else-exp
    (let ([expected-length (length '())]
          [field-names '()]
          [pred-names '()]
          [preds (list)])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'else-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '() args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'else-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'else-exp args))))
  (define while-exp
    (let ([expected-length (length '(conds bodies))]
          [field-names '(conds bodies)]
          [pred-names '(expression? (list-of expression?))]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x) ((list-of expression?) x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'while-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(conds bodies) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'while-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'while-exp args))))
  (define define-exp
    (let ([expected-length (length '(val body))]
          [field-names '(val body)]
          [pred-names '(symbol? expression?)]
          [preds (list
                   (lambda (x) (symbol? x))
                   (lambda (x) (expression? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'define-exp
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(val body) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'define-exp
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'define-exp args)))))


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

  (begin
    (define ignored
      (define-datatype:datatype-checker&registry-updater
        'environment
        '((empty-env-record)
           (extended-env-record
             (syms (list-of symbol?))
             (vals vector?)
             (env environment?)))))
    (define environment
      (cons
        '(empty-env-record extended-env-record)
        '((empty-env-record) (extended-env-record syms vals env))))
    (define environment?
      (if (symbol? 'environment)
          (lambda args
            (if (null? args)
                (define-datatype:report-error
                  'environment?
                  "expects 1 argument, not 0.")
                (if (null? (cdr args))
                    (let ([variant (car args)])
                      (let ([type-info environment])
                        (if (and (pair? type-info) (list? (car type-info)))
                            (and (pair? variant)
                                 (memq (car variant) (car type-info))
                                 #t)
                            (define-datatype:report-error
                              'environment?
                              (string-append
                                "did not get a data type bound to an "
                                "  appropriate structure: ~s. "
                                "  This tends to happen when the type name is "
                                "  bound to a lexical variable.")
                              'type-name
                              type-info))))
                    (define-datatype:report-error
                      'environment?
                      (string-append
                        "expects 1 argument, not ~s. "
                        "  With argument list = ~s.")
                      (length args)
                      args))))
          (define-datatype:report-error
            'environment
            "Type name is not a symbol: ~s."
            'type-name)))
    (define empty-env-record
      (let ([expected-length (length '())]
            [field-names '()]
            [pred-names '()]
            [preds (list)])
        (lambda args
          (if (not (= (length args) expected-length))
              (define-datatype:report-error 'empty-env-record
                (string-append
                  "Expected ~s arguments but got ~s arguments."
                  "   Fields are: ~s    Args are: ~s.")
                expected-length (length args) '() args))
          (for-each
            (lambda (a f p pname)
              (if (not (p a))
                  (define-datatype:report-error 'empty-env-record
                    "  Bad ~a field (~s ~s) => #f." f pname a)))
            args field-names preds pred-names)
          (cons 'empty-env-record args))))
    (define extended-env-record
      (let ([expected-length (length '(syms vals env))]
            [field-names '(syms vals env)]
            [pred-names '((list-of symbol?) vector? environment?)]
            [preds (list
                     (lambda (x) ((list-of symbol?) x))
                     (lambda (x) (vector? x))
                     (lambda (x) (environment? x)))])
        (lambda args
          (if (not (= (length args) expected-length))
              (define-datatype:report-error 'extended-env-record
                (string-append
                  "Expected ~s arguments but got ~s arguments."
                  "   Fields are: ~s    Args are: ~s.")
                expected-length (length args) '(syms vals env) args))
          (for-each
            (lambda (a f p pname)
              (if (not (p a))
                  (define-datatype:report-error 'extended-env-record
                    "  Bad ~a field (~s ~s) => #f." f pname a)))
            args field-names preds pred-names)
          (cons 'extended-env-record args)))))

					; datatype for procedures.  At first there is only one
					; kind of procedure, but more kinds will be added later.

          (begin
            (define ignored
              (define-datatype:datatype-checker&registry-updater
                'proc-val
                '((prim-proc (name symbol?))
                   (closure
                     (args
                       (lambda (x)
                         (or ((list-of symbol?) x)
                             (improper-list-of-symbols? x)
                             (symbol? x))))
                     (bodies (list-of expression?))
                     (env environment?))
                   (continuation-proc (arg continuation?)))))
            (define proc-val
              (cons
                '(prim-proc closure continuation-proc)
                '((prim-proc name)
                   (closure args bodies env)
                   (continuation-proc arg))))
            (define proc-val?
              (if (symbol? 'proc-val)
                  (lambda args
                    (if (null? args)
                        (define-datatype:report-error
                          'proc-val?
                          "expects 1 argument, not 0.")
                        (if (null? (cdr args))
                            (let ([variant (car args)])
                              (let ([type-info proc-val])
                                (if (and (pair? type-info) (list? (car type-info)))
                                    (and (pair? variant)
                                         (memq (car variant) (car type-info))
                                         #t)
                                    (define-datatype:report-error
                                      'proc-val?
                                      (string-append
                                        "did not get a data type bound to an "
                                        "  appropriate structure: ~s. "
                                        "  This tends to happen when the type name is "
                                        "  bound to a lexical variable.")
                                      'type-name
                                      type-info))))
                            (define-datatype:report-error
                              'proc-val?
                              (string-append
                                "expects 1 argument, not ~s. "
                                "  With argument list = ~s.")
                              (length args)
                              args))))
                  (define-datatype:report-error
                    'proc-val
                    "Type name is not a symbol: ~s."
                    'type-name)))
            (define prim-proc
              (let ([expected-length (length '(name))]
                    [field-names '(name)]
                    [pred-names '(symbol?)]
                    [preds (list (lambda (x) (symbol? x)))])
                (lambda args
                  (if (not (= (length args) expected-length))
                      (define-datatype:report-error 'prim-proc
                        (string-append
                          "Expected ~s arguments but got ~s arguments."
                          "   Fields are: ~s    Args are: ~s.")
                        expected-length (length args) '(name) args))
                  (for-each
                    (lambda (a f p pname)
                      (if (not (p a))
                          (define-datatype:report-error 'prim-proc
                            "  Bad ~a field (~s ~s) => #f." f pname a)))
                    args field-names preds pred-names)
                  (cons 'prim-proc args))))
            (define closure
              (let ([expected-length (length '(args bodies env))]
                    [field-names '(args bodies env)]
                    [pred-names '((lambda (x)
                                    (or ((list-of symbol?) x)
                                        (improper-list-of-symbols? x)
                                        (symbol? x)))
                                   (list-of expression?)
                                   environment?)]
                    [preds (list
                             (lambda (x)
                               ((lambda (x)
                                  (or ((list-of symbol?) x)
                                      (improper-list-of-symbols? x)
                                      (symbol? x)))
                                 x))
                             (lambda (x) ((list-of expression?) x))
                             (lambda (x) (environment? x)))])
                (lambda args
                  (if (not (= (length args) expected-length))
                      (define-datatype:report-error 'closure
                        (string-append
                          "Expected ~s arguments but got ~s arguments."
                          "   Fields are: ~s    Args are: ~s.")
                        expected-length (length args) '(args bodies env) args))
                  (for-each
                    (lambda (a f p pname)
                      (if (not (p a))
                          (define-datatype:report-error 'closure
                            "  Bad ~a field (~s ~s) => #f." f pname a)))
                    args field-names preds pred-names)
                  (cons 'closure args))))
            (define continuation-proc
              (let ([expected-length (length '(arg))]
                    [field-names '(arg)]
                    [pred-names '(continuation?)]
                    [preds (list (lambda (x) (continuation? x)))])
                (lambda args
                  (if (not (= (length args) expected-length))
                      (define-datatype:report-error 'continuation-proc
                        (string-append
                          "Expected ~s arguments but got ~s arguments."
                          "   Fields are: ~s    Args are: ~s.")
                        expected-length (length args) '(arg) args))
                  (for-each
                    (lambda (a f p pname)
                      (if (not (p a))
                          (define-datatype:report-error 'continuation-proc
                            "  Bad ~a field (~s ~s) => #f." f pname a)))
                    args field-names preds pred-names)
                  (cons 'continuation-proc args)))))


; Environment definitions for CSSE 304 Scheme interpreter.
