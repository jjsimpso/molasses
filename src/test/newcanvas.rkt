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
             get-size
             get-client-size
             get-virtual-size
             get-view-start
             show-scrollbars
             init-auto-scrollbars
             suspend-flush
             resume-flush
             flush
             get-canvas-background)

    ;; tentative mode options: 'plaintext, 'columnar, 'layout
    (define mode 'plaintext)

    ;; initially hide both scrollbars 
    (init-auto-scrollbars #f #f 0 0)
    
    ;; store the drawing context for efficiency
    (define dc (get-dc))
    
    ;; width of virtual canvas
    (define canvas-width 10)
    
    ;; cached scroll position of primary axis used for visible element determination
    (define scroll-position 0)

    ;; needed so that we can tell if the canvas is getting bigger or smaller during on-size events
    (define cached-client-width 10)
    (define cached-client-height 10)
    
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
           (when (dlist-advance-tail! cursor) ; returns false when tail can't advance any further
             (loop (dlist-tail-value cursor)))]
          [else
           (if (eq? (dlist-tail-prev cursor)
                    (dlist-head cursor))
               (set-dlist-tail! cursor #f)
               (set-dlist-tail! cursor (dlist-tail-prev cursor)))]))
      (set! visible-elements cursor)
      #;(printf "set-visible-elements ~a to ~a~n" (dlist-head-value visible-elements) (dlist-tail-value visible-elements)))

    (define (adjust-visible-elements-forward! cursor top bottom)
      ;; advance the tail to last visible element
      (let loop ([e (dlist-tail-value cursor)])
        (cond
          [(not e) void]
          [(element-visible? e top bottom)
           ;(printf "adjust-visible-elements-forward! loop~n")
           (when (dlist-advance-tail! cursor) ; returns false when tail can't advance any further
             (loop (dlist-tail-value cursor)))]
          [else
           (if (eq? (dlist-tail-prev cursor)
                    (dlist-head cursor))
               (set-dlist-tail! cursor #f)
               (set-dlist-tail! cursor (dlist-tail-prev cursor)))]))
      ;; advance the head while it isn't visible
      (for ([e (in-dlist visible-elements)]
            #:break (element-visible? e top bottom))
        ;(printf "adjust-visible-elements-forward! for~n")
        (dlist-advance-head! visible-elements))
      void)

    (define (adjust-visible-elements-back! cursor top bottom)
      ;; retreat the head to first visible element
      (let loop ([e (dlist-head-value cursor)])
        (cond
          [(not e) void] ; should never happen
          [(element-visible? e top bottom)
           (when (dlist-retreat-head! cursor)
             (loop (dlist-head-value cursor)))]
          [else
           (unless (not (dlist-tail cursor))
             (set-dlist-head! cursor (dlist-head-next cursor)))]))
      ;; retreat the tail until it is visible
      (let loop ([e (dlist-tail-value cursor)])
        (cond
          [(not e) void]
          [(element-visible? e top bottom) void]
          [else
           (when (dlist-retreat-tail! cursor)
             (loop (dlist-tail-value cursor)))]))
      void)
    
    ;;
    (define (update-visible-elements! scroll-change top bottom)
      ;(printf "update-visible-elements! ~a~n" scroll-change)
      (when visible-elements
        (define head-element (dlist-head-value visible-elements))
        (define tail-element (dlist-tail-value visible-elements))
        
        (if (> scroll-change 0)
            (if (and tail-element (element-visible? tail-element top bottom))
                (adjust-visible-elements-forward! visible-elements top bottom)
                (set-visible-elements!))
            (if (and head-element (element-visible? head-element top bottom))
                (adjust-visible-elements-back! visible-elements top bottom)
                (set-visible-elements!)))))
    
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

      (set! canvas-width max-linewidth)
      
      (when (or (not (equal? vw max-linewidth))
                (not (equal? vh y)))
        (define-values (cw ch) (get-client-size))
        (define horz-pixels max-linewidth)
        (define vert-pixels y)
        (show-scrollbars (> horz-pixels cw) (> vert-pixels ch))
        (init-auto-scrollbars horz-pixels vert-pixels hscroll vscroll)))

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
        (set! canvas-width (element-x2 e)))
      
      (when (or (> canvas-width vw)
                (> y vh))
        ;(printf "update-virtual-size: ~ax~a to ~ax~a~n" vw vh canvas-width y)
        (define-values (cw ch) (get-client-size))
        (define horz-pixels canvas-width)
        (define vert-pixels y)
        (show-scrollbars (> horz-pixels cw) (> vert-pixels ch))
        (init-auto-scrollbars horz-pixels
                              vert-pixels
                              hscroll
                              vscroll)))
    
    (define/override (on-paint)
      (define-values (cw ch) (get-client-size))
      (define-values (vw vh) (get-virtual-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left cw) (+ top ch)))

      ;(printf "on-paint ~ax~a of ~ax~a~n" cw ch vw vh)
      (when (not visible-elements)
        (set-visible-elements!))
      
      ;; check to see if view-start changed. if so, call update-visible-elements!
      (when (not (equal? scroll-position top))
        (update-visible-elements! (- top scroll-position) top bottom)
        (set! scroll-position top))
      
      ;; only draw visible elements
      (for ([e (in-dlist visible-elements)])
        (send (element-snip e)
              draw dc
              (element-x1 e) (element-y1 e)
              (element-x1 e) (element-y1 e)
              (+ (element-x1 e) cw) (+ (element-y1 e) ch)
              0 0 'no-caret)))

    (define/override (on-size width height)
      (define-values (cw ch) (get-client-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left cw) (+ top ch)))

      ;(define-values (w h) (get-size))
      ;(printf "on-size client=~ax~a window=~ax~a new=~ax~a~n" cw ch w h width height)

      ;; update visible elements
      (if (> ch cached-client-height)
          (adjust-visible-elements-forward! visible-elements top bottom)
          (adjust-visible-elements-back! visible-elements top bottom))
      (set! cached-client-width cw)
      (set! cached-client-height ch)
      
      (show-scrollbars (> canvas-width cw)
                       (> (element-y2 (dlist-tail-value elements))
                          ch)))
    
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

;(send canvas append-string highlander-text)
(send canvas append-string "\n\n")
(send canvas append-string "text\nwith lots\nof\nnewlines")
(let ([response (gopher-fetch "gopher.endangeredsoft.org" test-selector #\0 70)])
  (send canvas append-string (port->string (gopher-response-data-port response))))
(printf "append finished~n")
;(send canvas set-visible-elements!)
