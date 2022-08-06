#lang racket

(require racket/struct)

(require (for-syntax syntax/parse))

(provide dlist
         dlink
         dlist-new
         dlist-empty?
         dlist-append!
         dlist-push!
         dlist-pop!
         dlist-length
         dlist->list
         in-dlist)

;; head points to first element of the dlist
;; tail points to the last element of the dlist
;; if there is only one element in the dlist, tail will be #f
(struct dlist
  ([head #:mutable]
   [tail #:mutable])
  #:transparent)

(struct dlink
  (value
   [prev #:mutable]
   [next #:mutable])
  #:transparent)


(define (dlist-new)
  (dlist #f #f))

(define (dlist-empty? dl)
  (not (dlist-head dl)))

(define (dlist-append! dl value)
  (define old-tail (dlist-tail dl))
  (cond
    [old-tail
     (set-dlist-tail! dl (dlink value old-tail #f))
     (set-dlink-next! old-tail (dlist-tail dl))]
    [(dlist-head dl)
     (set-dlist-tail! dl (dlink value (dlist-head dl) #f))
     (set-dlink-next! (dlist-head dl) (dlist-tail dl))]
    [else
     (set-dlist-head! dl (dlink value #f #f))]))

(define (dlist-push! dl value)
  (define new-node (dlink value #f (dlist-head dl)))
  ;; set current head's prev link to the new head
  (when (dlist-head dl)
    (set-dlink-prev! (dlist-head dl) new-node)
    ;; if we push to a one element dlist, we need to update tail as well
    (when (not (dlist-tail dl))
      (set-dlist-tail! dl (dlink-next new-node))))
  ;; set new head
  (set-dlist-head! dl new-node))

;; pops and returns the first dlink's value or #f
(define (dlist-pop! dl)
  (define old-head (dlist-head dl))
  (when old-head
      ;; remove old head
      (set-dlist-head! dl (dlink-next old-head))
      ;; clear new head's prev link unless list is now empty
      (when (dlist-head dl)
        (set-dlink-prev! (dlist-head dl) #f)
        ;; if tail and head are the same, set dlist tail to #f
        (when (eq? (dlist-tail dl) (dlist-head dl))
          (set-dlist-tail! dl #f))))
  ;; return popped value or #f
  (if old-head
      (dlink-value old-head)
      #f))

(define (dlist-length dl)
  (let loop ([cursor (dlist-head dl)]
             [i 0])
    (if cursor
        (loop (dlink-next cursor)
              (add1 i))
        i)))

#;(define (dlist->list dl)
  (let loop ([cursor (or (dlist-tail dl)
                         (dlist-head dl))]
             [accum '()])
    (if cursor
        (loop (dlink-prev cursor)
              (cons (dlink-value cursor) accum))
        accum)))

(define (dlist->list dl)
  (define head (dlist-head dl))
  (let loop ([cursor (or (dlist-tail dl)
                         (dlist-head dl))]
             [accum '()])
    (if (not (eq? cursor head))
        (loop (dlink-prev cursor)
              (cons (dlink-value cursor) accum))
        (if head
            (cons (dlink-value head) accum)
            accum))))

(define (in-dlink/proc n)
  #f)

(define-sequence-syntax in-dlink
  (lambda () #'in-dlink/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(val) (_ dlink-expr)]
       #'[(val)
          (:do-in
           ([(node) dlink-expr])
           (unless (or (dlink? node)
                       (not node))
             (raise-type-error 'in-dlink "doubly linked list" node))
           ([n node])
           (dlink? n)
           ([(next val) (values (dlink-next n) (dlink-value n))])
           #true
           #true
           [next])]]
      [_ #false])))

(define (in-dlist/proc n)
  #f)

(define a-dlist (dlist-new))
(dlist-push! a-dlist 1)
(dlist-push! a-dlist 2)
(dlist-push! a-dlist 3)
(for ([d (in-dlink (dlist-head a-dlist))])
  (printf "~a~n" d))
(for ([d (in-dlist a-dlist)])
  (printf "~a~n" d))

(define-sequence-syntax in-dlist
  (lambda () #'in-dlist/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(a) (_ expr)]
       #'[(a) (in-dlink (and (dlist? expr)
                             (dlist-head expr)))]])))

(module+ test (require rackunit)
  (define a-dlist (dlist-new))
  (dlist-append! a-dlist 3)
  (dlist-append! a-dlist 4)
  (dlist-append! a-dlist 5)
  (dlist-push! a-dlist 2)
  (dlist-push! a-dlist 1)
  (check-equal? (dlist->list a-dlist) '(1 2 3 4 5))
  (check-equal? (dlist-length a-dlist) 5)
  
  (dlist-pop! a-dlist)
  (check-equal? (dlist->list a-dlist) '(2 3 4 5))
  
  (check-equal? (dlist-pop! a-dlist) 2)
  (check-equal? (dlist-pop! a-dlist) 3)
  (check-equal? (dlist-pop! a-dlist) 4)
  (check-equal? (dlist-pop! a-dlist) 5)
  
  (check-equal? (dlist->list a-dlist) '())
  (check-equal? (dlist-empty? a-dlist) #t)

  (dlist-push! a-dlist 'a)
  (check-equal? (dlist->list a-dlist) '(a))

  (dlist-push! a-dlist 'b)
  (check-equal? (dlist->list a-dlist) '(b a))
  a-dlist)
