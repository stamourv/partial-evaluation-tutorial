#lang typed/racket

(require "exp.rkt")
(require/typed "prim.rkt" [prim (Op (Listof Val) -> Val)])

;; naive partial evaluator

(define-type Env (Listof (Pair Symbol Expr)))

(provide peval)
(define: (peval [program : Prog]) : Expr
  (match-define (Prog fdefs main) program)
  
  (define: (peval-expr [expr : Expr] [env : Env]) : Expr
    (match expr
      [(Const val) expr]
      [(Var var)   (match (assq var env)
                     [`(,_ . ,e) e]
                     [#f         expr])] ; dynamic, leave as is
      [(Prim op es)
       (define rs
         (for/list: : (Listof Expr) ([e : Expr es]) (peval-expr e env)))
       (if (andmap Const? rs) ; all values
           (Const (prim op (map Const-val rs)))
           (Prim op rs))]
      [(If test then else)
       (match (peval-expr test env)
         [(Const #t) (peval-expr then env)]
         [(Const #f) (peval-expr else env)]
         [test*      (If test* (peval-expr then env) (peval-expr else env))])]
      [(Apply f es)
       (define-values (args body)
         (match (assq f fdefs)
           [`(,_ . ,(Func args body)) (values args body)]
           [#f                        (error "unbound variable" f)]))
       (define new-env
         (append (for/list: : Env ([a : Symbol args] [e : Expr es])
                            (cons a (peval-expr e env)))
                 env))
       (peval-expr body new-env)]))

  (peval-expr main empty))


(module* test typed/racket
  (require typed/rackunit "exp.rkt" (submod "..") "eval.rkt")
 
  (define: (peval-no-env [e : Expr]) : Expr (peval (Prog '() e)))
  
  (check-equal? (peval-no-env (Const 0)) (Const 0))
  (check-equal? (peval-no-env (Prim '= `(,(Const 0) ,(Const 0)))) (Const #t))
  (check-equal? (peval-no-env (Prim '= `(,(Const 0) ,(Const 1)))) (Const #f))
  (check-equal? (peval-no-env (Prim '+ `(,(Const 0) ,(Const 1)))) (Const 1))
  (check-equal? (peval-no-env
                 (If (Prim '= `(,(Const 0) ,(Const 0))) (Const 1) (Const 2)))
                (Const 1))

  (define base-env
    `((exp
       . ,(Func '(x n)
                (If (Prim '= `(,(Var 'n) ,(Const 0)))
                    (Const 1)
                    (Prim '* `(,(Var 'x)
                               ,(Apply 'exp
                                       `(,(Var 'x)
                                         ,(Prim '- `(,(Var 'n)
                                                     ,(Const 1))))))))))))

  (define exp-prog
    (Prog base-env (Apply 'exp `(,(Const 2) ,(Const 3)))))
    
  (check-equal? (eval (Prog base-env (peval exp-prog))) 8)
  (check-equal? (peval exp-prog) (Const 8))

  (define exp-prog2
    (Prog base-env (Apply 'exp `(,(Var 'x) ,(Const 3)))))

  ;; initial env only contains function definitions, so no way to provide a
  ;; value for `x' and run
  (check-equal? (peval exp-prog2)
                (Prim '* `(,(Var 'x)
                           ,(Prim '* `(,(Var 'x)
                                       ,(Prim '* `(,(Var 'x) ,(Const 1))))))))

  ;; won't terminate on that one
  (define exp-prog3
    (Prog base-env (Apply 'exp `(,(Const 2) ,(Var 'n))))))
