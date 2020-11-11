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
        (syntax-expand (let-exp vars vals (append (map (lambda (x y) (set!-exp x y)) vars vals) bodies)))]
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


(define make-k
  (lambda (v) v))

(define top-level-eval
  (lambda (form)
    (eval-exp-cps form (empty-env) (init-k))))

(begin
  (define ignored
    (define-datatype:datatype-checker&registry-updater
      'continuation
      '((init-k) (list-k) (not-k)
         (app-exp-1-k
           (rands (list-of expression?))
           (env environment?)
           (k continuation?))
         (app-exp-2-k
           (proc-val proc-val?)
           (env environment?)
           (k continuation?))
         (if-one-exp-k
           (body expression?)
           (env environment?)
           (k continuation?))
         (if-exp-k
           (body1 expression?)
           (body2 expression?)
           (env environment?)
           (k continuation?))
         (while-exp-k-1
           (bodies (list-of expression?))
           (exp expression?)
           (env environment?)
           (k continuation?))
         (while-exp-k-2
           (exp expression?)
           (env environment?)
           (k continuation?))
         (set!-exp-k
           (var symbol?)
           (env environment?)
           (k continuation?))
         (define-exp-k (var symbol?) (k continuation?))
         (eval-bodies-cps-k
           (lst (list-of expression?))
           (env environment?)
           (k continuation?))
         (map-cps-k-1 (proc procedure?) (L list?) (k continuation?))
         (map-cps-k-2 (proc (lambda (x) #t)) (k continuation?))
         (our-map-cps-k-1
           (proc proc-val?)
           (ls (lambda (x) #t))
           (env environment?)
           (k continuation?))
         (our-map-cps-k-2 (var (lambda (x) #t)) (k continuation?)))))
  (define continuation
    (cons
      '(init-k list-k not-k app-exp-1-k app-exp-2-k if-one-exp-k if-exp-k
         while-exp-k-1 while-exp-k-2 set!-exp-k define-exp-k
         eval-bodies-cps-k map-cps-k-1 map-cps-k-2 our-map-cps-k-1
         our-map-cps-k-2)
      '((init-k) (list-k) (not-k) (app-exp-1-k rands env k)
         (app-exp-2-k proc-val env k) (if-one-exp-k body env k)
         (if-exp-k body1 body2 env k)
         (while-exp-k-1 bodies exp env k) (while-exp-k-2 exp env k)
         (set!-exp-k var env k) (define-exp-k var k)
         (eval-bodies-cps-k lst env k) (map-cps-k-1 proc L k)
         (map-cps-k-2 proc k) (our-map-cps-k-1 proc ls env k)
         (our-map-cps-k-2 var k))))
  (define continuation?
    (if (symbol? 'continuation)
        (lambda args
          (if (null? args)
              (define-datatype:report-error
                'continuation?
                "expects 1 argument, not 0.")
              (if (null? (cdr args))
                  (let ([variant (car args)])
                    (let ([type-info continuation])
                      (if (and (pair? type-info) (list? (car type-info)))
                          (and (pair? variant)
                               (memq (car variant) (car type-info))
                               #t)
                          (define-datatype:report-error
                            'continuation?
                            (string-append
                              "did not get a data type bound to an "
                              "  appropriate structure: ~s. "
                              "  This tends to happen when the type name is "
                              "  bound to a lexical variable.")
                            'type-name
                            type-info))))
                  (define-datatype:report-error
                    'continuation?
                    (string-append
                      "expects 1 argument, not ~s. "
                      "  With argument list = ~s.")
                    (length args)
                    args))))
        (define-datatype:report-error
          'continuation
          "Type name is not a symbol: ~s."
          'type-name)))
  (define init-k
    (let ([expected-length (length '())]
          [field-names '()]
          [pred-names '()]
          [preds (list)])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'init-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '() args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'init-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'init-k args))))
  (define list-k
    (let ([expected-length (length '())]
          [field-names '()]
          [pred-names '()]
          [preds (list)])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'list-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '() args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'list-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'list-k args))))
  (define not-k
    (let ([expected-length (length '())]
          [field-names '()]
          [pred-names '()]
          [preds (list)])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'not-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '() args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'not-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'not-k args))))
  (define app-exp-1-k
    (let ([expected-length (length '(rands env k))]
          [field-names '(rands env k)]
          [pred-names '((list-of expression?)
                         environment?
                         continuation?)]
          [preds (list
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'app-exp-1-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(rands env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'app-exp-1-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'app-exp-1-k args))))
  (define app-exp-2-k
    (let ([expected-length (length '(proc-val env k))]
          [field-names '(proc-val env k)]
          [pred-names '(proc-val? environment? continuation?)]
          [preds (list
                   (lambda (x) (proc-val? x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'app-exp-2-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(proc-val env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'app-exp-2-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'app-exp-2-k args))))
  (define if-one-exp-k
    (let ([expected-length (length '(body env k))]
          [field-names '(body env k)]
          [pred-names '(expression? environment? continuation?)]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'if-one-exp-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(body env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'if-one-exp-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'if-one-exp-k args))))
  (define if-exp-k
    (let ([expected-length (length '(body1 body2 env k))]
          [field-names '(body1 body2 env k)]
          [pred-names '(expression?
                         expression?
                         environment?
                         continuation?)]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x) (expression? x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'if-exp-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(body1 body2 env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'if-exp-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'if-exp-k args))))
  (define while-exp-k-1
    (let ([expected-length (length '(bodies exp env k))]
          [field-names '(bodies exp env k)]
          [pred-names '((list-of expression?)
                         expression?
                         environment?
                         continuation?)]
          [preds (list
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) (expression? x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'while-exp-k-1
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(bodies exp env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'while-exp-k-1
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'while-exp-k-1 args))))
  (define while-exp-k-2
    (let ([expected-length (length '(exp env k))]
          [field-names '(exp env k)]
          [pred-names '(expression? environment? continuation?)]
          [preds (list
                   (lambda (x) (expression? x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'while-exp-k-2
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(exp env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'while-exp-k-2
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'while-exp-k-2 args))))
  (define set!-exp-k
    (let ([expected-length (length '(var env k))]
          [field-names '(var env k)]
          [pred-names '(symbol? environment? continuation?)]
          [preds (list
                   (lambda (x) (symbol? x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'set!-exp-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(var env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'set!-exp-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'set!-exp-k args))))
  (define define-exp-k
    (let ([expected-length (length '(var k))]
          [field-names '(var k)]
          [pred-names '(symbol? continuation?)]
          [preds (list
                   (lambda (x) (symbol? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'define-exp-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(var k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'define-exp-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'define-exp-k args))))
  (define eval-bodies-cps-k
    (let ([expected-length (length '(lst env k))]
          [field-names '(lst env k)]
          [pred-names '((list-of expression?)
                         environment?
                         continuation?)]
          [preds (list
                   (lambda (x) ((list-of expression?) x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'eval-bodies-cps-k
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(lst env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'eval-bodies-cps-k
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'eval-bodies-cps-k args))))
  (define map-cps-k-1
    (let ([expected-length (length '(proc L k))]
          [field-names '(proc L k)]
          [pred-names '(procedure? list? continuation?)]
          [preds (list
                   (lambda (x) (procedure? x))
                   (lambda (x) (list? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'map-cps-k-1
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(proc L k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'map-cps-k-1
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'map-cps-k-1 args))))
  (define map-cps-k-2
    (let ([expected-length (length '(proc k))]
          [field-names '(proc k)]
          [pred-names '((lambda (x) #t) continuation?)]
          [preds (list
                   (lambda (x) ((lambda (x) #t) x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'map-cps-k-2
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(proc k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'map-cps-k-2
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'map-cps-k-2 args))))
  (define our-map-cps-k-1
    (let ([expected-length (length '(proc ls env k))]
          [field-names '(proc ls env k)]
          [pred-names '(proc-val?
                         (lambda (x) #t)
                         environment?
                         continuation?)]
          [preds (list
                   (lambda (x) (proc-val? x))
                   (lambda (x) ((lambda (x) #t) x))
                   (lambda (x) (environment? x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'our-map-cps-k-1
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(proc ls env k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'our-map-cps-k-1
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'our-map-cps-k-1 args))))
  (define our-map-cps-k-2
    (let ([expected-length (length '(var k))]
          [field-names '(var k)]
          [pred-names '((lambda (x) #t) continuation?)]
          [preds (list
                   (lambda (x) ((lambda (x) #t) x))
                   (lambda (x) (continuation? x)))])
      (lambda args
        (if (not (= (length args) expected-length))
            (define-datatype:report-error 'our-map-cps-k-2
              (string-append
                "Expected ~s arguments but got ~s arguments."
                "   Fields are: ~s    Args are: ~s.")
              expected-length (length args) '(var k) args))
        (for-each
          (lambda (a f p pname)
            (if (not (p a))
                (define-datatype:report-error 'our-map-cps-k-2
                  "  Bad ~a field (~s ~s) => #f." f pname a)))
          args field-names preds pred-names)
        (cons 'our-map-cps-k-2 args)))))


(define apply-k
  (lambda (k v)
    (if (not (continuation? k))
      (k v)
      (cases continuation k
        [init-k () v]
        [list-k () (list v)]
        [not-k () (not v)]
        [app-exp-1-k (rands env k) (eval-rands-cps rands env (app-exp-2-k v env k))]
        [app-exp-2-k (proc-val env k) (apply-proc-cps proc-val v env k)]
        [if-one-exp-k (body env k) (if v
                                           (eval-exp-cps body env k)
                                           (apply-k k #t))]
        [if-exp-k (body1 body2 env k) (if v
                                        (eval-exp-cps body1 env k)
                                        (eval-exp-cps body2 env k))]
        [while-exp-k-1 (bodies exp env k) (if v
                                        (eval-bodies-cps bodies env (while-exp-k-2 exp env k)))]
        [while-exp-k-2 (exp env k) (eval-exp-cps exp env k)]
        [set!-exp-k (var env k) (apply-k k (set-val env var v))]
        [define-exp-k (var k) (apply-k k (define-val var v init-env))]
        [eval-bodies-cps-k (lst env k) (eval-bodies-cps (cdr lst) env k)]
        [map-cps-k-1 (proc L k) (map-cps proc (cdr L) (map-cps-k-2 v k))]
        [map-cps-k-2 (val k) (apply-k k (cons val v))]
        [our-map-cps-k-1 (proc ls env k) (our-map-cps proc (cdr ls) env (our-map-cps-k-2 v k))]
        [our-map-cps-k-2 (var k) (apply-k k (cons var v))]))))

(define our-map-cps
  (lambda (proc ls env k)
    (if (null? ls)
      (apply-k k '())
      (apply-proc-cps proc (list (car ls)) env (our-map-cps-k-1 proc ls env k)))))


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
        (eval-exp-cps rator env (app-exp-1-k rands env k))]
     [if-one-exp (condition body)
        (eval-exp-cps condition env (if-one-exp-k body env k))]
	   [if-exp (condition body1 body2)
         (eval-exp-cps condition env (if-exp-k body1 body2 env k))]
	   [lambda-exp (vars bodies)
		       (apply-k k (closure vars bodies env))]
     [while-exp (conds bodies)
      (eval-exp-cps conds env (while-exp-k-1 bodies exp env k))]
     [set!-exp (var val)
      (eval-exp-cps val env (set!-exp-k var env k))]
     [define-exp (var body)
     (eval-exp-cps body env (define-exp-k var k))]
	   [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))


(define eval-bodies-cps
 (lambda (lst env k)
   (cond
    [(null? (cdr lst)) (eval-exp-cps (car lst) env k)]
    [else (eval-exp-cps (car lst) env (eval-bodies-cps-k lst env k))])))


(define map-cps
  (lambda (proc-cps L k)
    (if (null? L)
      (apply-k k '())
      (if (pair? L)
        (proc-cps (car L) (map-cps-k-1 proc-cps L k))))))
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
     [continuation-proc (k) (apply-k k (car args))]
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
            list-tail product call/cc exit-list load))


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
      [(apply) (our-apply-cps (1st args) (cadr args) env k)]
      [(quotient) (apply-k k (apply quotient args))]
      [(member) (apply-k k (apply member args))]
      [(product) (apply-k k (apply product args))]
      [(union) (apply-k k (apply union args))]
      [(call/cc) (our-call/cc args env k)]
      [(exit-list) args]
      [(load) (our-load args)]
      [else (error 'apply-prim-proc-cps
		   "Bad primitive procedure name: ~s"
		   prim-op)])))


(define (our-call/cc args env k)
  (let [(continuation (continuation-proc k))]
    (apply-proc-cps (car args) (list continuation) env k)))

(define (our-load args)
  (let loop ([file (open-input-file (car args))])
    (if (not (port-eof? file))
    (eval-one-exp (get-datum file))
    (loop file))))

(define union ; s1 and s2 are sets of symbols.
  (lambda (s1 s2)
    (let loop ([s1 s1])
      (cond [(null? s1) s2]
            [(memq (car s1) s2) (loop (cdr s1))]
            [else (cons (car s1) (loop (cdr s1)))]))))


(define our-apply-cps
  (lambda (proc args env k)
    (apply-proc-cps proc args env k)))

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
  (if (list? (2nd datum))
      (define-exp (car (2nd datum)) (lambda-exp (cdr (2nd datum)) (map parse-exp (cddr datum))))
      (define-exp (2nd datum) (parse-exp (3rd datum)))))

(define (improper-list-map proc list)
  (if (pair? x)
      (cons (proc (car list)) (improper-list-map proc (cdr list)))
      (proc x)))

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
