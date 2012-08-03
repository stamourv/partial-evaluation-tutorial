#lang typed/racket

(provide (all-defined-out))

(struct: Prog ([defs : (Listof FDef)] [body : Expr]))

(struct: FDef ([name : Symbol] [args : (Listof Symbol)] [body : Expr]))

(define-type Val (U Integer Boolean))
(define-predicate Val? Val)

(define-type Expr (U Const Var Apply Prim If))
(define-predicate Expr? Expr)
(struct: Const ([val : Val]))
(struct: Var   ([var : Symbol]))
(struct: Apply ([op : Symbol] [args : (Listof Expr)]))
(struct: Prim  ([op : Op]     [args : (Listof Expr)]))
(struct: If    ([test : Expr] [then : Expr] [else : Expr]))

(define-type Op (U '= '+ '- '*))
(define-predicate Op? Op)
