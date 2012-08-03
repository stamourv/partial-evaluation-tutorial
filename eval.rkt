#lang racket

(require "exp.rkt")

;; evaluator (not partial)

;; (define-type Env (Listof (Pair Symbol Val)))
(define Env? (listof (cons/c symbol? Val?)))

(define/contract (eval program) (-> Prog? Val?)
  (match-define (Prog fdefs main) program)
  
  (define/contract (eval-expr expr env) (-> Expr? Env? Val?)
    0)

  (eval-expr main empty))


(module+ test
  (require rackunit)

  (define base-env
    `(,(FDef 'exp '(x n)
             (If (Prim '= `(,(Var 'n) ,(Const 0)))
                 (Const 1)
                 (Prim '* `(,(Var 'x) ,(Prim '- `(,(Var 'n) ,(Const 1)))))))))

  (define exp-prog
    (Prog base-env (Apply 'exp `(,(Const 2) ,(Const 3)))))
    
  (check-equal? (eval exp-prog) 8))
