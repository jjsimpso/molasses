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
       [cache-ystart #:auto #:mutable]
       [cache-yend #:auto #:mutable])
      #:prefab #:auto-value 0)
    
    ;; list of all elements in order of insertion
    (define elements (dlist-new))

    #;(define (first-visible-element)
      )
    
    (define/override (on-paint)
      (define-values (cw ch) (get-client-size))
      (define-values (vw vh) (get-virtual-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left cw) (+ top ch)))
      ;; calculate the current scrollbar positions in 0.0-1.0 range for init-auto-scrollbars
      (define-values (hscroll vscroll)
        (let-values ([(x y) (get-view-start)])
          (values (/ x vw)
                  (/ y vh))))
      
      (printf "on-paint ~ax~a of ~ax~a~n" cw ch vw vh)
      (define max-linewidth 0)
      (define snip-w (box 0))
      (define snip-h (box 0))
      ;; current drawing position
      (define x 0)
      (define y 0)
      (for ([e (in-dlist elements)])
        (set-element-cache-ystart! e y)
        (when (and (<= (element-cache-ystart e) bottom)
                   (>= (element-cache-yend e) top))
          (send (element-snip e) draw dc x y x y (+ x cw) (+ y ch) 0 0 'no-caret))
        (send (element-snip e) get-extent dc x y snip-w snip-h #f #f #f #f)
        (set! y (inexact->exact (+ y (unbox snip-h))))
        (set-element-cache-yend! e y)
        (when (> (+ x (unbox snip-w))
                 max-linewidth)
          (set! max-linewidth (inexact->exact (+ x (unbox snip-w))))))

      (when (or (not (equal? vw max-linewidth))
                (not (equal? vh y)))
        (init-auto-scrollbars max-linewidth y hscroll vscroll)))
    
    (define/public (append-string s [alignment 'left])
      (case mode
        [(plaintext)
         ;; for plaintext mode, insert each line in string as an element
         (define p (open-input-string s))
         (for ([line (in-lines p)])
           (dlist-append! elements (element (make-object string-snip% line) alignment)))]
        [else
         (dlist-append! elements (element (make-object string-snip% s) alignment))]))
      

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
