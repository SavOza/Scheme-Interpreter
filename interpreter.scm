(define get-operator (lambda (op-symbol)
  (cond
    ((eq? op-symbol '+) +)
    ((eq? op-symbol '*) *)
    ((eq? op-symbol '-) -)
    ((eq? op-symbol '/) /)
    (else "ERROR"))))

(define get-value (lambda (var env)
    (cond 
       ( (null? env) "ERROR" )
       ( (eq? var (caar env)) (cadar env))
       ( else (get-value var (cdr env))))))

(define extend-env (lambda (var val old-env)
        (append (list (list var val)) old-env)))

(define extend-env-list (lambda (varlist vallist env)
      (cond
         ((not (= (length varlist) (length vallist))) "ERROR")
         ((null? varlist) '())
         (else (append (list (list (car varlist) (s7 (car vallist) env))) (extend-env-list (cdr varlist) (cdr vallist) env)))
      )
   )
)

(define define-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'define) (symbol? (cadr e)))))
         
(define if-expr? (lambda (e)
         (and (list? e) (= (length e) 4) (eq? (car e) 'if))))
         
(define let-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'let) (list? (cadr e)) (list? (caddr e)))))

(define base-operator? (lambda (e)
         (or (eq? e '+) (eq? e '-) (eq? e '*) (eq? e '/))))

(define lambda-expr? (lambda (e)
         (and (list? e) (= (length e) 3) (eq? (car e) 'lambda) (list? (cadr e)) (list? (caddr e)))))


(define s7 (lambda (e env)
   (cond
      ( (string? e) "ERROR")
      ( (number? e) e)
      ( (base-operator? e) "[PROCEDURE]") 
      ( (symbol? e)  (if (list? (get-value e env))
                        "[PROCEDURE]"
                        (get-value e env)
                     )
      
      )
      ( (not (list? e)) "ERROR")
      ( (not (> (length e) 1)) "ERROR")
      ( (if-expr? e) 
         (if (eq? 0 (s7 (cadr e) env))
            (s7 (cadddr e) env)
            (s7 (caddr e) env)
         )
      )
      ( (let-expr? e) 
         (if (check-let-bindcount (cadr e) '())
            (if (string? (bind-let (cadr e) env))
               "ERROR"
               (s7 (caddr e) (append (bind-let (cadr e) env) env))
            )
            "ERROR"
         )
      )
      ( (base-operator? (car e)) 
         (let (
                (operator (get-operator (car e)))
                (operands (map s7 (cdr e) (make-list (length (cdr e) ) env )))
              )
              (apply operator operands))
      )
      (else (lambda-prepare e env))
   ))
)

(define bind-let (lambda (e env)
   (cond
      ((null? e) e)
      ((number? (cadar e)) (append (list (car e)) (bind-let (cdr e) env)))
      ((symbol? (cadar e)) (append (list (list (caar e) (s7 (cadar e) env))) (bind-let (cdr e) env)))
      (else "ERROR")
   )

))

(define check-let-bindcount (lambda (e env)
   (cond
      ((null? e) #t)
      ((not (list? e)) #f)
      ((not (list? (car e))) #f)
      ((not (= (length (car e)) 2)) #f)
      (else (and (check-let-existence (car (car e)) env) (check-let-bindcount (cdr e) (append (list (car (car e))) env))))
   )

))

(define check-let-existence (lambda (e env)
   (cond
      ((not (symbol? e)) #f)
      ((null? env) #t)
      ((eq? e (car env)) #f)
      (else (check-let-existence e (cdr env)))
   )

))

(define lambda-prepare (lambda (e env)
   (cond
      ((lambda-expr? (car e)) 
         (if (string? (extend-env-list (cadar e) (cdr e) env)) 
            "ERROR" 
            (s7 (caddar e) (append (extend-env-list (cadar e) (cdr e) env) env))
         )
      )
      ((symbol? (car e))
         (if (string? (get-value (car e) env))
            "ERROR"
            (if (lambda-expr? (get-value (car e) env))
               (if (string? (extend-env-list (cadr (get-value (car e) env)) (cdr e) env)) 
                  "ERROR" 
                  (s7 (caddr (get-value (car e) env)) (append (extend-env-list (cadr (get-value (car e) env)) (cdr e) env) env))
               )
               "ERROR"
            )
         )
      )
   ))
)

(define repl (lambda (env)
   (let* (
           (dummy1 (display "cs305> "))
           (expr (read))
           (new-env (if (define-expr? expr)
                        (cond 
                           ((lambda-expr? (caddr expr)) (extend-env (cadr expr) (caddr expr) env))
            
                           (else (extend-env (cadr expr) (s7 (caddr expr) env) env))
                        )
                        env
                    ))
           (val (if (define-expr? expr)
                    (cadr expr)
                    (s7 expr env)
                ))
           (dummy2 (display "Value: "))
           (dummy3 (display val))
           (dummy4 (newline))
           (dummy5 (newline))
          )
          (repl new-env))))


(define cs305 (lambda () (repl '())))
