#lang racket

(provide memoize)

(define (memoize fun [hash-type make-hash])
  (define ht (hash-type)) 
  (lambda (arg)
    (define cache-val (hash-ref ht arg #f))
    (cond
      [cache-val
       ;(printf "retrieving cached value~n")
       cache-val]
      [else
       (define val (fun arg))
       (hash-set! ht arg val)
       val])))
