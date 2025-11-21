#lang racket

(provide ecma-csi-tokenize
         ecma-csi-remove)

;; these three functions taken from Dominik Joe Pantůček:
;; https://gitlab.com/racketeer/uni-table/-/blob/master/private/ecma-csi.rkt?ref_type=heads
(define (ecma-csi-tokenize str)
  (regexp-match* #rx"\e\\[[0-9;]*.|.[^\e]*" str))

(define (ecma-csi-start? str)
  (and (> (string-length str) 1)
       (eq? (string-ref str 0) #\u1b)
       (eq? (string-ref str 1) #\[)))

(define (ecma-csi-remove str)
  (string-join
   (filter (compose not ecma-csi-start?)
           (ecma-csi-tokenize str))
   ""))
