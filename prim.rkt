#lang racket

(require "exp.rkt")

(provide prim)

(define (prim op vals)
  (apply (match op ['= =] ['+ +] ['- -] ['* *] [_ (error "invalid prim" op)])
         vals))
