#lang typed/racket

(provide (all-defined-out))

(struct: Prog ([defs : (Listof FDef)] [body : Expr]) #:transparent)

(define-type FDef (Pair Symbol Func))
(struct: Func ([args : (Listof Symbol)] [body : Expr]) #:transparent)

(define-type Val (U Integer Boolean))
(define-predicate Val? Val)

(define-type Expr (U Const Var Apply Prim If))
(define-predicate Expr? Expr)
(struct: Const ([val : Val])                               #:transparent)
(struct: Var   ([var : Symbol])                            #:transparent)
(struct: Apply ([op : Symbol] [args : (Listof Expr)])      #:transparent)
(struct: Prim  ([op : Op]     [args : (Listof Expr)])      #:transparent)
(struct: If    ([test : Expr] [then : Expr] [else : Expr]) #:transparent)

(define-type Op (U '= '+ '- '*))
(define-predicate Op? Op)
