#lang racket

(provide memoize
         memoize-2args)

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

(define (memoize-2args fun [hash-type make-hash])
  (define ht (hash-type)) 
  (lambda (arg1 arg2)
    (define cache-val (hash-ref ht (cons arg1 arg2) #f))
    (cond
      [cache-val
       (printf "retrieving cached value for ~a,~a~n" arg1 arg2)
       cache-val]
      [else
       (define val (fun arg1 arg2))
       (hash-set! ht (cons arg1 arg2) val)
       val])))
