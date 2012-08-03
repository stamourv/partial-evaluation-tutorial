#lang racket

(require "exp.rkt")

;; evaluator (not partial)

;; (define-type Env (Listof (Pair Symbol Val)))
(define Env? (listof (cons/c symbol? Val?)))

(define/contract (eval program) (-> Prog? Val?)
  (match-define (Prog fdefs main) program)
  
  (define/contract (eval-expr expr env) (-> Expr? Env? Val?)
    (match expr
      [(Const val) val]
      [(Var var)   (match (assq var env)
                     [`(,_ . ,val) val]
                     [#f           (error "unbound variable" var)])]
      [(Prim op es)
       (define rs (for/list ([e es]) (eval-expr e env)))
       (prim op rs)]
      [(If test then else)
       (if (eval-expr test env)
           (eval-expr then env)
           (eval-expr else env))]
      [(Apply f es)
       (match-define (FDef _ args body) (lookup-func f fdefs))
       (define new-env
         (append (for/list ([a args] [e es]) (cons a (eval-expr e env)))
                 env))
       (eval-expr body new-env)]))

  (eval-expr main empty))

(provide prim)
(define/contract (prim op vals) (-> Op? (listof Val?) Val?)
  (apply (match op ['= =] ['+ +] ['- -] ['* *])
         vals))


(module+ test
  (require rackunit)

  (define (eval-no-env e) (eval (Prog '() e)))
  
  (check-equal? (eval-no-env (Const 0)) 0)
  (check-equal? (eval-no-env (Prim '= `(,(Const 0) ,(Const 0)))) #t)
  (check-equal? (eval-no-env (Prim '= `(,(Const 0) ,(Const 1)))) #f)
  (check-equal? (eval-no-env (Prim '+ `(,(Const 0) ,(Const 1)))) 1)
  (check-equal? (eval-no-env
                 (If (Prim '= `(,(Const 0) ,(Const 0))) (Const 1) (Const 2)))
                1)

  (define base-env
    `(,(FDef 'exp '(x n)
             (If (Prim '= `(,(Var 'n) ,(Const 0)))
                 (Const 1)
                 (Prim '* `(,(Var 'x)
                            ,(Apply 'exp
                                    `(,(Var 'x)
                                      ,(Prim '- `(,(Var 'n)
                                                  ,(Const 1)))))))))))

  (define exp-prog
    (Prog base-env (Apply 'exp `(,(Const 2) ,(Const 3)))))
    
  (check-equal? (eval exp-prog) 8))
