#lang racket

(require racket/struct)

(require (for-syntax syntax/parse))

(provide dlist
         dlink
         dlist-new
         dlist-empty?
         dlist-head-value
         dlist-tail-value
         dlist-append!
         dlist-push!
         dlist-pop!
         dlist-length
         dlist->list
         in-dlist
         dlist-cursor
         dlist-advance-head
         dlist-retreat-head
         dlist-advance-tail
         dlist-retreat-tail)

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

(define (dlist-head-value dl)
  (if (dlist-head dl)
      (dlink-value (dlist-head dl))
      #f))

(define (dlist-tail-value dl)
  (if (dlist-tail dl)
      (dlink-value (dlist-tail dl))
      #f))

(define (dlist-append! dl value)
  (define old-tail (dlist-tail dl))
  (cond
    [(and old-tail (dlink-next old-tail))
     ;; prevent appending if tail is not the last element of the dlist
     ;; this could be because dl points to list structure shared by another dlist
     (raise-argument-error 'dlist-append! "(not (dlink-next (dlist-tail dl)))" dl)]
    [old-tail
     (set-dlist-tail! dl (dlink value old-tail #f))
     (set-dlink-next! old-tail (dlist-tail dl))]
    [(dlist-head dl)
     (set-dlist-tail! dl (dlink value (dlist-head dl) #f))
     (set-dlink-next! (dlist-head dl) (dlist-tail dl))]
    [else
     (set-dlist-head! dl (dlink value #f #f))]))

(define (dlist-push! dl value)
  (define old-head (dlist-head dl))
  (cond
    [(and old-head (dlink-prev old-head))
     ;; prevent pushing if head is not at the beginning of the dlist
     ;; this could be because dl points to list structure shared by another dlist
     (raise-argument-error 'dlist-pop! "(not (dlink-prev (dlist-head dl)))" dl)]
    [old-head
     (define new-node (dlink value #f old-head))
     ;; set current head's prev link to the new head
     (set-dlink-prev! old-head new-node)
     ;; if we push to a one element dlist, we need to update tail as well
     (when (not (dlist-tail dl))
       (set-dlist-tail! dl (dlink-next new-node)))
     ;; set new head
     (set-dlist-head! dl new-node)]
    [else
     (set-dlist-head! dl (dlink value #f #f))]))

;; pops and returns the first dlink's value or #f
(define (dlist-pop! dl)
  (define old-head (dlist-head dl))
  (cond
    [(and old-head (dlink-prev old-head))
     ;; prevent popping if head is not the first element of the dlist
     ;; this could be because dl points to list structure shared by another dlist
     (raise-argument-error 'dlist-pop! "(not (dlink-prev (dlist-head dl)))" dl)]
    [old-head
     ;; remove old head
     (set-dlist-head! dl (dlink-next old-head))
     ;; clear new head's prev link unless list is now empty
     (when (dlist-head dl)
       (set-dlink-prev! (dlist-head dl) #f)
       ;; if tail and head are the same, set dlist tail to #f
       (when (eq? (dlist-tail dl) (dlist-head dl))
         (set-dlist-tail! dl #f)))]
    [else void])
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
      [[(val) (_ dlink-expr tail-expr)]
       #'[(val)
          (:do-in
           ([(node tail) (values dlink-expr tail-expr)])
           (unless (or (dlink? node)
                       (false? node))
             (raise-argument-error 'in-dlink "dlink?" node))
           ([n node])
           (dlink? n)
           ([(next val) (values (if (eq? n tail)
                                    #f
                                    (dlink-next n))
                                (dlink-value n))])
           #true
           #true
           [next])]]
      [_ #false])))

(define (in-dlist/proc n)
  #f)

(define-sequence-syntax in-dlist
  (lambda () #'in-dlist/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(a) (_ expr)]
       #'[(a) (in-dlink (and (dlist? expr)
                             (dlist-head expr))
                        (and (dlist? expr)
                             (dlist-tail expr)))]])))


;;; Functions to manipulate a cursor over a portion of an existing dlist
;;; Need to be careful with these functions. Modifying the underlying dlist
;;; could cause unexpected behavior.

;; returns a new dlist to use as a cursor for an existing dlist
(define (dlist-cursor dl)
  (dlist (dlist-head dl) (dlist-tail dl)))

;; advances the head pointer to the next link if it exists
;; returns the new head or #f if head didn't change
(define (dlist-advance-head dl)
  (cond
    [(and (dlist-head dl) (dlink-next (dlist-head dl)))
     (set-dlist-head! dl (dlink-next (dlist-head dl)))
     (when (eq? (dlist-head dl) (dlist-tail dl))
       (set-dlist-tail! dl #f))
     (dlist-head dl)]
    [else #f]))

;; moves the head pointer back one link, but stops if already at beginning
;; returns the new head or #f if head didn't change
(define (dlist-retreat-head dl)
  (cond
    [(and (dlist-head dl) (dlink-prev (dlist-head dl)))
     (set-dlist-head! dl (dlink-prev (dlist-head dl)))
     (dlist-head dl)]
    [else #f]))

;; advances the tail pointer to the next link, but stops if already at the end
;; returns the new tail or #f if tail didn't change
(define (dlist-advance-tail dl)
  (cond
    [(and (dlist-tail dl) (dlink-next (dlist-tail dl)))
     (set-dlist-tail! dl (dlink-next (dlist-tail dl)))
     (dlist-tail dl)]
    [else #f]))

;; moves the tail pointer back one link
;; returns the new tail or #f if there was no tail or if the dlist now has one element
(define (dlist-retreat-tail dl)
  (cond
    [(dlist-tail dl)
     (set-dlist-tail! dl (dlink-prev (dlist-tail dl)))
     (when (eq? (dlist-tail dl) (dlist-head dl))
       ;; one element dlists only have a head
       (set-dlist-tail! dl #f))
     (dlist-tail dl)]
    [else #f]))


(module+ test (require rackunit)
  (define a-dlist (dlist-new))
  (dlist-append! a-dlist 3)
  (check-equal? (dlink-value (dlist-head a-dlist)) 3)
  (check-equal? (dlist-tail a-dlist) #f)
  (dlist-append! a-dlist 4)
  (check-equal? (dlink-value (dlist-head a-dlist)) 3)
  (check-equal? (dlink-value (dlist-tail a-dlist)) 4)
  (dlist-append! a-dlist 5)
  (dlist-push! a-dlist 2)
  (check-equal? (dlink-value (dlist-head a-dlist)) 2)
  (dlist-push! a-dlist 1)
  (check-equal? (dlink-value (dlist-head a-dlist)) 1)
  (check-equal? (dlist->list a-dlist) '(1 2 3 4 5))
  (check-equal? (dlist-length a-dlist) 5)
  (check-equal? (for/list ([v (in-dlist a-dlist)]) v) '(1 2 3 4 5))
  
  
  ;;; cursor tests
  (define cursor (dlist-cursor a-dlist))
  (check-equal? (dlist-head-value cursor) 1)
  (check-equal? (dlist-tail-value cursor) 5)
  
  (dlist-advance-head cursor)
  (check-equal? (dlist-head-value cursor) 2)
  (check-equal? (dlist-tail-value cursor) 5)
  (check-equal? (for/list ([v (in-dlist cursor)]) v) '(2 3 4 5))
  ;; check pop and push exceptions from a cursor that is no longer pointing to the head
  ;; of the original dlist
  (check-equal? 
   (with-handlers [(exn:fail:contract? (lambda (e) #t))]
     (dlist-pop! cursor))
   #t)
  (check-equal? 
   (with-handlers [(exn:fail:contract? (lambda (e) #t))]
     (dlist-push! cursor 0))
   #t)
  (dlist-retreat-head cursor)

  (check-equal? (dlist-head-value cursor) 1)
  (check-equal? (dlist-tail-value cursor) 5)
  (check-equal? (dlist-retreat-head cursor) #f)
  (check-equal? (dlist-advance-tail cursor) #f)
 
  (dlist-retreat-tail cursor)
  (check-equal? (dlist-tail-value cursor) 4)
  (check-equal? 
   (with-handlers [(exn:fail:contract? (lambda (e) #t))]
     (dlist-append! cursor 6))
   #t)
  (check-equal? (dlist->list cursor) '(1 2 3 4))
  (check-equal? (for/list ([v (in-dlist cursor)]) v) '(1 2 3 4))
  (dlist-advance-tail cursor)
  (check-equal? (dlist-tail-value cursor) 5)
  ;;; ----------
  
  (dlist-pop! a-dlist)
  (check-equal? (dlist->list a-dlist) '(2 3 4 5))
  (check-equal? (dlink-value (dlist-head cursor)) 1)
  
  (check-equal? (dlist-pop! a-dlist) 2)
  (check-equal? (dlist-pop! a-dlist) 3)
  (check-equal? (dlist-pop! a-dlist) 4)
  (check-equal? (dlist-pop! a-dlist) 5)
  
  (check-equal? (dlist->list a-dlist) '())
  (check-equal? (dlist-empty? a-dlist) #t)
  (check-equal? (dlist-pop! a-dlist) #f)
  
  (dlist-push! a-dlist 'a)
  (check-equal? (dlist->list a-dlist) '(a))

  (dlist-push! a-dlist 'b)
  (check-equal? (dlist->list a-dlist) '(b a))
  a-dlist)
