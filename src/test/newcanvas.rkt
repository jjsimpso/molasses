#lang racket/gui

(require "../dlist.rkt"
         "../gopher.rkt")

#;(require mred/private/mrcanvas
         mred/private/wx
         mred/private/wxcanvas
         (prefix-in wx: mred/private/wxme/text))


(define frame 
  (new (class frame% (super-new))
       [label "Layout canvas test"]
       [width 800]
       [height 600]))

(define layout-canvas%
  (class canvas% (super-new)
    (inherit get-dc
             get-client-size
             get-virtual-size
             get-view-start
             init-auto-scrollbars
             suspend-flush
             resume-flush
             flush
             get-canvas-background)

    ;; tentative mode options: 'plaintext, 'columnar, 'layout
    (define mode 'plaintext)

    ;; store the drawing context for efficiency
    (define dc (get-dc))

    ;; an element is a snip with a horizontal alignment
    ;; alignment can be 'left, 'right, or 'center
    (struct element
      ([snip #:mutable]
       [alignment #:mutable]
       [x1 #:auto #:mutable] ; upper left coords
       [y1 #:auto #:mutable]
       [x2 #:auto #:mutable] ; lower right coords
       [y2 #:auto #:mutable])
      #:prefab #:auto-value 0)
    
    ;; list of all elements in order of insertion
    (define elements (dlist-new))

    (define visible-elements #f)

    (define (element-visible? e top bottom)
      (and (<= (element-y1 e) bottom)
           (>= (element-y2 e) top)))
    
    (define/public (set-visible-elements!)
      (define-values (cw ch) (get-client-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left cw) (+ top ch)))

      (define cursor (dlist-cursor elements))
      ;; find first visible element
      (for ([e (in-dlist cursor)]
            #:break (element-visible? e top bottom))
        (dlist-advance-head! cursor))
      ;; set cursor's tail to element after first visible and advance tail until we reach the
      ;; last visible elment
      (set-dlist-tail! cursor (dlist-head-next cursor))
      (let loop ([e (dlist-tail-value cursor)])
        (cond
          [(not e) void]
          [(element-visible? e top bottom)
           (dlist-advance-tail! cursor)
           (loop (dlist-tail-value cursor))]
          [else
           (if (eq? (dlist-tail-prev cursor)
                    (dlist-head cursor))
               (set-dlist-tail! cursor #f)
               (set-dlist-tail! cursor (dlist-tail-prev cursor)))]))
      (set! visible-elements cursor)
      (printf "set-visible-elements ~a to ~a~n" (dlist-head-value visible-elements) (dlist-tail-value visible-elements)))

    ;; iterate over all elements and calculate virtual size of canvas
    (define (calculate-virtual-size)
      (define-values (vw vh) (get-virtual-size))
      ;; calculate the current scrollbar positions in 0.0-1.0 range for init-auto-scrollbars
      (define-values (hscroll vscroll)
        (let-values ([(x y) (get-view-start)])
          (values (/ x vw)
                  (/ y vh))))
      (define max-linewidth 0)
      (define snip-w (box 0))
      (define snip-h (box 0))
      ;; current drawing position
      (define x 0)
      (define y 0)
      (for ([e (in-dlist elements)])
        (set-element-x1! e x)
        (set-element-y1! e y)
        (send (element-snip e) get-extent dc x y snip-w snip-h #f #f #f #f)
        (set! y (inexact->exact (+ y (unbox snip-h))))
        (set-element-x2! e (inexact->exact (+ x (unbox snip-w))))
        (set-element-y2! e y)
        (when (> (element-x2 e) max-linewidth)
          (set! max-linewidth (element-x2 e))))

      (when (or (not (equal? vw max-linewidth))
                (not (equal? vh y)))
        (init-auto-scrollbars max-linewidth y hscroll vscroll)))

    ;; update virtual size of canvas
    ;; e is the new element and must be the tail of the element list
    ;; previous is the previous tail of the element list
    (define (update-virtual-size e previous)
      (define-values (vw vh) (get-virtual-size))
      ;; calculate the current scrollbar positions in 0.0-1.0 range for init-auto-scrollbars
      (define-values (hscroll vscroll)
        (let-values ([(x y) (get-view-start)])
          (values (/ x vw)
                  (/ y vh))))
      (define max-linewidth 0)
      (define snip-w (box 0))
      (define snip-h (box 0))
      ;; current drawing position
      (define-values (x y)
        (if previous
            (values 0 (element-y2 previous))
            (values 0 0)))

      (set-element-x1! e x)
      (set-element-y1! e y)
      (send (element-snip e) get-extent dc x y snip-w snip-h #f #f #f #f)
      (set! y (inexact->exact (+ y (unbox snip-h))))
      (set-element-x2! e (inexact->exact (+ x (unbox snip-w))))
      (set-element-y2! e y)
      (when (> (element-x2 e) vw)
        (set! max-linewidth (element-x2 e)))
      
      (when (or (> max-linewidth vw)
                (> y vh))
        ;(printf "update-virtual-size: ~ax~a to ~ax~a~n" vw vh max-linewidth y)
        (init-auto-scrollbars (max max-linewidth vw)
                              (max y vh)
                              hscroll
                              vscroll)))
    
    (define/override (on-paint)
      (define-values (cw ch) (get-client-size))
      (define-values (vw vh) (get-virtual-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left cw) (+ top ch)))

      (printf "on-paint ~ax~a of ~ax~a~n" cw ch vw vh)
      (for ([e (in-dlist elements)])
        (when (element-visible? e top bottom)
          (send (element-snip e) draw dc (element-x1 e) (element-y1 e) (element-x1 e) (element-y1 e) (+ (element-x1 e) cw) (+ (element-y1 e) ch) 0 0 'no-caret))))
    
    (define/public (append-string s [alignment 'left])
      (case mode
        [(plaintext)
         ;; for plaintext mode, insert each line in string as an element
         (define p (open-input-string s))
         (for ([line (in-lines p)])
           (define e (element (make-object string-snip% line) alignment))
           (update-virtual-size e (dlist-tail-value elements))
           (dlist-append! elements e))]
        [else
         (define e (element (make-object string-snip% s) alignment))
         (update-virtual-size e (dlist-tail-value elements))
         (dlist-append! elements e)]))
      

    ))

(define canvas
  (new layout-canvas% (parent frame)
       (style '(hscroll vscroll resize-corner))
       ))

(send frame show #t)

(define highlander-text "He is immortal. Born in the highlands of Scotland 400 years ago, there are others like him. Some good, some evil. For centuries he has battled the forces of darkness with holy ground his only refuge. He cannot die unless you take his head and with it his power. There can be only one. He is Duncan Macleod, the Highlander!\n")
(define test-selector "gamefaqs-archive/ps2/final-fantasy-xii/FAQ_Walkthrough-by--Berserker.txt")
;(define test-selector ".")

(send canvas append-string highlander-text)
(send canvas append-string "\n\n")
(send canvas append-string "text\nwith lots\nof\nnewlines")
(let ([response (gopher-fetch "gopher.endangeredsoft.org" test-selector #\0 70)])
  (send canvas append-string (port->string (gopher-response-data-port response))))
(printf "append finished~n")
;(send canvas set-visible-elements!)
