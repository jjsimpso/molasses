#lang racket

(require racket/struct
         racket/unsafe/ops)

(require (for-syntax syntax/parse))

(provide dlist
         dlink
         dlink-next
         dlink-prev
         dlink-value
         dlist-head
         dlist-tail
         set-dlist-head!
         set-dlist-tail!
         dlist-new
         dlist-empty?
         dlist-head-value
         dlist-tail-value
         dlist-head-next
         dlist-tail-prev
         dlist-ref
         dlist-append!
         dlist-push!
         dlist-pop!
         dlist-length
         dlist->list
         in-dlist
         in-dlist-reverse
         dlist-cursor
         dlist-peek-tail-next
         dlist-peek-head-prev
         dlist-advance-head!
         dlist-retreat-head!
         dlist-advance-tail!
         dlist-retreat-tail!
         dlist-advance-head-while!
         dlist-retreat-head-while!
         dlist-advance-tail-while!
         dlist-retreat-tail-while!)

;; head points to first element of the dlist, a dlink
;; tail points to the last element of the dlist, a dlink
;; if there is only one element in the dlist, tail will be #f
(struct dlist
  ([head #:mutable]
   [tail #:mutable])
  #:transparent)

(struct dlink
  (value
   [prev #:mutable]
   [next #:mutable])
  #:transparent #:authentic)


(define (dlist-new)
  (dlist #f #f))

(define (dlist-empty? dl)
  (not (dlist-head dl)))

;; returns the value of the first element in the dlist, or #f
(define (dlist-head-value dl)
  (if (dlist-head dl)
      (dlink-value (dlist-head dl))
      #f))

;; returns the value of the last element in the dlist, which could actually be the value
;; pointed to by HEAD if the dlist only has one element
(define (dlist-tail-value dl)
  (if (dlist-tail dl)
      (dlink-value (dlist-tail dl))
      (dlist-head-value dl)))

;; returns a dlink or #f
(define (dlist-head-next dl)
  (if (dlist-head dl)
      (dlink-next (dlist-head dl))
      #f))

;; returns a dlink or #f
(define (dlist-tail-prev dl)
  (if (dlist-tail dl)
      (dlink-prev (dlist-tail dl))
      #f))

;; returns the value of element at index or #f
;; ideally i'd like this to throw an exception instead of returning #f, as list-ref does
(define (dlist-ref dl index)
  (for/or ([node (in-dlist dl)]
           [i (in-naturals)]
           #:when (= i index))
    node))

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
  (define head (dlist-head dl))
  (define tail (dlist-tail dl))
  (cond
    [(not head) 0]
    [(not tail) 1]
    [else
     (let loop ([cursor head]
                [i 0])
       (if (eq? cursor tail)
           (add1 i)
           (loop (dlink-next cursor)
                 (add1 i))))]))

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
                                    (unsafe-struct*-ref n 2)) ; next
                                (unsafe-struct*-ref n 0))])   ; value
           #true
           tail  ; stop condition for cursors with only one element (tail will be false)
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


(define (in-dlink-reverse/proc n)
  #f)

(define-sequence-syntax in-dlink-reverse
  (lambda () #'in-dlink-reverse/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(val) (_ dlink-expr head-expr)]
       #'[(val)
          (:do-in
           ([(node tail) (values dlink-expr head-expr)])
           (unless (or (dlink? node)
                       (false? node))
             (raise-argument-error 'in-dlink-reverse "dlink?" node))
           ([n node])
           (dlink? n)
           ([(next val) (values (if (eq? n tail)
                                    #f
                                    (dlink-prev n))
                                (dlink-value n))])
           #true
           #true
           [next])]]
      [_ #false])))

(define (in-dlist-reverse/proc n)
  #f)

(define-sequence-syntax in-dlist-reverse
  (lambda () #'in-dlist-reverse/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(a) (_ expr)]
       #'[(a) (in-dlink-reverse (and (dlist? expr)
                                     (or (dlist-tail expr) (dlist-head expr)))
                                (and (dlist? expr)
                                     (dlist-head expr)))]])))

;;; Functions to manipulate a cursor over a portion of an existing dlist
;;; Need to be careful with these functions. Modifying the underlying dlist
;;; could cause unexpected behavior.

;; returns a new dlist to use as a cursor for an existing dlist
(define (dlist-cursor dl)
  (dlist (dlist-head dl) (dlist-tail dl)))

(define (dlist-peek-tail-next dl)
  (define tail (dlist-tail dl))
  (if (false? tail)
      (let ([head (dlist-head dl)])
        (and head (dlink-next head) (dlink-value (dlink-next head))))
      (let ([tail-next (dlink-next tail)])
        (and tail-next (dlink-value tail-next)))))

(define (dlist-peek-head-prev dl)
  (define head (dlist-head dl))
  (if head
      (let ([head-prev (dlink-prev head)])
        (and head-prev (dlink-value head-prev)))
      #f))

;; advances the head pointer to the next link if it exists
;; returns void or #f if head didn't change
(define (dlist-advance-head! dl)
  (cond
    [(and (dlist-head dl) (dlink-next (dlist-head dl)))
     (set-dlist-head! dl (dlink-next (dlist-head dl)))
     (when (eq? (dlist-head dl) (dlist-tail dl))
       (set-dlist-tail! dl #f))
     (dlist-head dl)]
    [else #f]))

;; moves the head pointer back one link, but stops if already at beginning
;; returns the new head or #f if head didn't change
(define (dlist-retreat-head! dl)
  (cond
    [(and (dlist-head dl) (dlink-prev (dlist-head dl)))
     (set-dlist-head! dl (dlink-prev (dlist-head dl)))
     (when (not (dlist-tail dl))
       (set-dlist-tail! dl (dlist-head-next dl)))
     (dlist-head dl)]
    [else #f]))

;; advances the tail pointer to the next link, but stops if already at the end
;; returns the new tail or #f if tail didn't change
(define (dlist-advance-tail! dl)
  (define tail (dlist-tail dl))
  (cond
    [(and tail (dlink-next tail))
     (set-dlist-tail! dl (dlink-next tail))
     (dlist-tail dl)]
    [(and (false? tail) (dlink-next (dlist-head dl)))
     ;; dl is a cursor currently pointing to only one element
     (set-dlist-tail! dl (dlink-next (dlist-head dl)))]
    [else #f]))

;; moves the tail pointer back one link
;; returns the new tail or #f if there was no tail or if the dlist now has one element
(define (dlist-retreat-tail! dl)
  (cond
    [(dlist-tail dl)
     (set-dlist-tail! dl (dlink-prev (dlist-tail dl)))
     (when (eq? (dlist-tail dl) (dlist-head dl))
       ;; one element dlists only have a head
       (set-dlist-tail! dl #f))
     (dlist-tail dl)]
    [else #f]))

(define (dlist-advance-head-while! dl pred? #:stop-before-false? [ sbf? #f])
  ;(printf "advance head value = ")
  (let loop ([cnt 0])
    ;(printf "~a, " (dlist-head-value dl))
    (cond
      [(dlist-empty? dl)
       cnt]
      [(false? (pred? (dlist-head-value dl)))
       (when (and sbf? (> cnt 0))
         ;(printf "retreat head~n")
         (dlist-retreat-head! dl))
       cnt]
      [else
       (dlist-advance-head! dl)
       (loop (add1 cnt))])))

(define (dlist-retreat-head-while! dl pred? #:stop-before-false? [sbf? #f])
  (let loop ([cnt 0])
    (cond
      [(false? (pred? (dlist-head-value dl)))
       (when (and sbf? (> cnt 0))
         (dlist-advance-head! dl))
       cnt]
      [(false? (dlist-peek-head-prev dl))
       cnt]
      [else
       (dlist-retreat-head! dl)
       (loop (add1 cnt))])))

(define (dlist-advance-tail-while! dl pred? #:stop-before-false? [sbf? #f])
  (let loop ([cnt 0])
    (cond
      [(false? (pred? (dlist-tail-value dl)))
       (when (and sbf? (> cnt 0))
         (dlist-retreat-tail! dl))
       cnt]
      [(false? (dlist-peek-tail-next dl))
       cnt]
      [else
       (dlist-advance-tail! dl)
       (loop (add1 cnt))])))

(define (dlist-retreat-tail-while! dl pred? #:stop-before-false? [sbf? #f])
  (let loop ([cnt 0])
    (cond
      [(dlist-empty? dl)
       cnt]
      [(false? (pred? (dlist-tail-value dl)))
       (when (and sbf? (> cnt 0))
         (dlist-advance-tail! dl))
       cnt]
      ;; this returns when the dlist only has a head (one element)
      ;; should we allow retreating the tail until we have an empty dlist?
      [(false? (dlist-tail dl))
       cnt]
      [else
       (dlist-retreat-tail! dl)
       (loop (add1 cnt))])))

(module+ test (require rackunit)
  (define a-dlist (dlist-new))
  (check-equal? (for/list ([v (in-dlist a-dlist)]) v) '())
  (check-equal? (for/list ([v (in-dlist-reverse a-dlist)]) v) '())
  (dlist-append! a-dlist 3)
  (check-equal? (for/list ([v (in-dlist a-dlist)]) v) '(3))
  (check-equal? (for/list ([v (in-dlist-reverse a-dlist)]) v) '(3))
  (check-equal? (dlink-value (dlist-head a-dlist)) 3)
  (check-equal? (dlist-tail a-dlist) #f)
  (check-equal? (dlist-head-value a-dlist) 3)
  (check-equal? (dlist-tail-value a-dlist) 3)
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
  (check-equal? (for/list ([v (in-dlist-reverse a-dlist)]) v) '(5 4 3 2 1))
  (check-equal? (dlist-ref a-dlist 0) 1)
  (check-equal? (dlist-ref a-dlist 4) 5)
  (check-equal? (dlist-ref a-dlist 5) #f)
  
  ;;; cursor tests
  (define cursor (dlist-cursor a-dlist))
  (check-equal? (dlist-head-value cursor) 1)
  (check-equal? (dlist-tail-value cursor) 5)
  
  (dlist-advance-head! cursor)
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
  (dlist-retreat-head! cursor)

  (check-equal? (dlist-head-value cursor) 1)
  (check-equal? (dlist-tail-value cursor) 5)
  (check-equal? (dlist-retreat-head! cursor) #f)
  (check-equal? (dlist-advance-tail! cursor) #f)
  (check-equal? (dlist-peek-tail-next cursor) #f)
  (check-equal? (dlist-peek-head-prev cursor) #f)
  
  (dlist-retreat-tail! cursor)
  (check-equal? (dlist-tail-value cursor) 4)
  (check-equal? (dlist-peek-tail-next cursor) 5)
  (check-equal? (dlist-length cursor) 4)
  (check-equal? (for/list ([v (in-dlist cursor)]) v) '(1 2 3 4))
  (check-equal? 
   (with-handlers [(exn:fail:contract? (lambda (e) #t))]
     (dlist-append! cursor 6))
   #t)
  (check-equal? (dlist->list cursor) '(1 2 3 4))
  (check-equal? (for/list ([v (in-dlist cursor)]) v) '(1 2 3 4))
  (dlist-retreat-tail! cursor)
  (dlist-retreat-tail! cursor)
  (dlist-retreat-tail! cursor)
  (check-equal? (dlist->list cursor) '(1))
  (check-equal? (for/list ([v (in-dlist cursor)]) v) '(1))
  (check-equal? (dlist-tail-value cursor) 1)
  (check-equal? (dlist-peek-tail-next cursor) 2)
  (dlist-advance-tail! cursor)
  (dlist-advance-tail! cursor)
  (dlist-advance-tail! cursor)
  (dlist-advance-tail! cursor)
  (check-equal? (dlist-tail-value cursor) 5)

  ; test advancing head until dlist has only one element, then retreat head
  (dlist-advance-head! cursor)
  (dlist-advance-head! cursor)
  (dlist-advance-head! cursor)
  (check-equal? (dlist-head-value cursor) 4)
  (check-equal? (dlist-tail-value cursor) 5)
  (check-equal? (dlist-peek-head-prev cursor) 3)
  (dlist-advance-head! cursor)
  (check-equal? (dlist-tail cursor) #f)
  (check-equal? (dlist-head-value cursor) 5)
  (check-equal? (dlist-tail-value cursor) 5)
  (dlist-advance-head! cursor)
  (check-equal? (dlist-head-value cursor) 5)
  (dlist-retreat-head! cursor)
  (check-equal? (dlist-head-value cursor) 4)
  (check-equal? (dlist-tail-value cursor) 5)
  (dlist-retreat-head! cursor)
  (dlist-retreat-head! cursor)
  (dlist-retreat-head! cursor)
  
  (check-equal? (dlist->list cursor) '(1 2 3 4 5))
  (check-equal? (dlist-retreat-head-while! cursor (lambda (v) (> v 2))) 0)
  (check-equal? (dlist-head-value cursor) 1)
  (check-equal? (dlist-tail-value cursor) 5)
  (dlist-advance-head-while! cursor (lambda (v) (< v 5)))
  (check-equal? (dlist-head-value cursor) 5)
  (dlist-retreat-head-while! cursor (lambda (v) (> v 2)))
  (check-equal? (dlist-head-value cursor) 2)
  (dlist-retreat-head-while! cursor (lambda (v) #t))
  (check-equal? (dlist->list cursor) '(1 2 3 4 5))
  (dlist-retreat-tail-while! cursor (lambda (v) (> v 3)))
  (check-equal? (dlist-tail-value cursor) 3)
  (dlist-advance-tail-while! cursor (lambda (v) (= v 3)))
  (check-equal? (dlist-tail-value cursor) 4)
  (dlist-advance-tail-while! cursor (lambda (v) #t))
  (check-equal? (dlist->list cursor) '(1 2 3 4 5))
  (dlist-retreat-tail-while! cursor (lambda (v) #t))
  (check-equal? (dlist-head-value cursor) 1)
  (check-equal? (dlist-tail-value cursor) 1)
  (dlist-advance-tail-while! cursor (lambda (v) (< v 5)) #:stop-before-false? #t)
  (check-equal? (dlist-tail-value cursor) 4)
  (dlist-retreat-tail-while! cursor (lambda (v) (> v 1)) #:stop-before-false? #t)
  (check-equal? (dlist-tail-value cursor) 2)
  (dlist-advance-tail-while! cursor (lambda (v) (< v 5)))
  (check-equal? (dlist->list cursor) '(1 2 3 4 5))
  (dlist-advance-head-while! cursor (lambda (v) (< v 4)) #:stop-before-false? #t)
  (check-equal? (dlist-head-value cursor) 3)
  (dlist-retreat-head-while! cursor (lambda (v) (> v 1)) #:stop-before-false? #t)
  (check-equal? (dlist-head-value cursor) 2)
  (dlist-retreat-head-while! cursor (lambda (v) (> v 1)))
  (check-equal? (dlist-head-value cursor) 1)
  (check-equal? (dlist->list cursor) '(1 2 3 4 5))
  
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
