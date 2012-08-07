#lang typed/racket

(require "exp.rkt")
(require/typed "prim.rkt" [prim (Op (Listof Val) -> Val)])
(require/typed racket/dict
  [dict-ref (All (A) ((Listof (Pair Symbol A)) Symbol (-> A) -> A))]
  [dict-set (All (A) ((Listof (Pair Symbol A)) Symbol A
                      -> (Listof (Pair Symbol A))))]
  [dict-has-key? (All (A) (Listof (Pair Symbol A)) Symbol -> Boolean)]
  [dict-update (All (A) ((Listof (Pair Symbol A)) Symbol (A -> A)
                         -> (Listof (Pair Symbol A))))])

;; final partial evaluator

(define-type Env (Listof (Pair Symbol Val)))

(define dummy-f (Func '() (Const #f))) ; placeholder

(provide peval)
(define: (peval [program : Prog]) : Prog
  (match-define (Prog fdefs main) program)
  
  (define: (peval-expr [expr : Expr] [env : Env]) : Expr
    (match expr
      [(Const val) expr]
      [(Var var)
       (define val (dict-ref env var (lambda () #f)))
       (if val (Const val) expr)] ; dynamic, leave as is
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
       (match-define (Func args body)
         (dict-ref fdefs f (lambda () (error "unbound variable" f))))
       (define es* (map (lambda: ([e : Expr]) (peval-expr e env)) es))
       ;; determine static and dynamic arguments
       (define z   (map (inst cons Symbol Expr) args es*))
       (define-values (sas* das)
         (partition (lambda: ([p : (Pair Symbol Expr)]) (Const? (cdr p))) z))
       (define sas
         (map (match-lambda [`(,(? symbol? a) . ,(Const val)) (cons a val)])
              sas*))
       (define new-env (append sas env))
       (cond [(null? das) ; completely static application, inline
              (peval-expr body new-env)]
             [else
              (define new-f (string->symbol (format "~a ~a" f sas)))
              ;; we specialize once for given inputs
              (unless (dict-has-key? fdefs new-f)
                (set! fdefs (dict-set fdefs new-f dummy-f)) ; placeholder
                (define new-body (peval-expr body new-env))
                ;; replace placeholder with actual definition
                (set! fdefs (dict-set fdefs new-f
                                      (Func (map (inst car Symbol Expr) das)
                                            new-body))))
              ;; apply specialized function
              (Apply new-f (map (inst cdr Symbol Expr) das))])]))

  (define: residual : Expr (peval-expr main empty))
  (Prog fdefs residual)) ; fdefs now includes residual defs


(module* test typed/racket
  (require typed/rackunit "exp.rkt" (submod "..") "eval.rkt")
 
  (define: (peval-no-env [e : Expr]) : Prog (peval (Prog '() e)))
  
  (check-equal? (peval-no-env (Const 0))
                (Prog '() (Const 0)))
  (check-equal? (peval-no-env (Prim '= `(,(Const 0) ,(Const 0))))
                (Prog '() (Const #t)))
  (check-equal? (peval-no-env (Prim '= `(,(Const 0) ,(Const 1))))
                (Prog '() (Const #f)))
  (check-equal? (peval-no-env (Prim '+ `(,(Const 0) ,(Const 1))))
                (Prog '() (Const 1)))
  (check-equal? (peval-no-env
                 (If (Prim '= `(,(Const 0) ,(Const 0))) (Const 1) (Const 2)))
                (Prog '() (Const 1)))

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
    
  (check-equal? (eval (peval exp-prog)) 8)
  (check-equal? (peval exp-prog) (Prog base-env (Const 8)))

  (define exp-prog2
    (Prog base-env (Apply 'exp `(,(Var 'x) ,(Const 3)))))

  ;; initial env only contains function definitions, so no way to provide a
  ;; value for `x' and run
  (check-equal?
   (peval exp-prog2)
   (Prog
    `(,@base-env
      (|exp ((n . 3))| .
            ,(Func '(x) (Prim '* `(,(Var 'x)
                                   ,(Apply '|exp ((n . 2))| `(,(Var 'x)))))))
      (|exp ((n . 2))| .
            ,(Func '(x) (Prim '* `(,(Var 'x)
                                   ,(Apply '|exp ((n . 1))| `(,(Var 'x)))))))
      (|exp ((n . 1))| .
            ,(Func '(x) (Prim '* `(,(Var 'x)
                                   ,(Apply '|exp ((n . 0))| `(,(Var 'x)))))))
      (|exp ((n . 0))| . ,(Func '(x) (Const 1))))
    (Apply '|exp ((n . 3))| `(,(Var 'x)))))

  (define exp-prog3
    (Prog base-env (Apply 'exp `(,(Const 2) ,(Var 'n)))))
  (check-equal?
   (peval exp-prog3)
   (Prog
    `(,@base-env
      (|exp ((x . 2))| .
            ,(Func '(n)
                   (If (Prim '= `(,(Var 'n) ,(Const 0)))
                       (Const 1)
                       (Prim '* `(,(Const 2)
                                  ,(Apply '|exp ((x . 2))|
                                          `(,(Prim '- `(,(Var 'n)
                                                        ,(Const 1)))))))))))
    (Apply '|exp ((x . 2))| `(,(Var 'n))))))
