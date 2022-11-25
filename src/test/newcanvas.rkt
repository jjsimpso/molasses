#lang racket/gui

(require "../dlist.rkt"
         "../gopher.rkt"
         "../config.rkt")

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

    ;; (left x top) is upper left corner of box
    (struct bounding-box
      (left top right bottom)
      #:prefab)

    ;; checks if bounding-box bb is at least partially within the range top to bottom
    (define (within-vertical-range? bb top bottom)
      (and (<= (bounding-box-top bb) bottom)
           (>= (bounding-box-bottom bb) top)))

    ;; checks if bounding-box bb is at least partially within the range left to right
    (define (within-horizontal-range? bb left right)
      (and (<= (bounding-box-left bb) right)
           (>= (bounding-box-right bb) left)))
    
    ;; an element is a snip with a horizontal alignment
    ;; alignment can be 'left, 'right, or 'center
    (struct element
      ([snip #:mutable]
       [alignment #:mutable]
       [bb-list #:auto #:mutable]) ; list of bounding boxes which cover element 
      #:prefab #:auto-value #f)

    (define (element-bottom e)
      (define bottom 0)
      (for/last ([bb (in-list (element-bb-list e))])
        (when (> (bounding-box-bottom bb) bottom)
          (set! bottom (bounding-box-bottom bb)))
        bottom))
    
    ;; list of all elements in order of insertion
    (define elements (dlist-new))

    (define visible-elements #f)

    (define styles (new style-list%))
    (send styles new-named-style "Standard" (send styles find-named-style "Basic"))

    ;; style to use if no style is specified
    ;; this can be changed with set-default-style before appending a string to set its snip to this style
    (define default-style (send styles find-named-style "Standard"))

    (define (element-visible? e top bottom)
      (for/first ([bb (in-list (element-bb-list e))]
                  #:when (within-vertical-range? bb top bottom))
        #t))
    
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
    #;(define (calculate-virtual-size)
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

    (define (calc-drawing-position previous-element)
      (if previous-element
          (let ([end-bb
                 (for/last ([bb (in-list (element-bb-list previous-element))])
                   bb)])
            ;; todo: check end-bb
            (case mode
              [(plaintext)
               (values 0 (bounding-box-bottom end-bb))]
              [else
               (values (bounding-box-right end-bb)
                       (bounding-box-bottom end-bb))]))
          (values 0 0)))
    
    ;; places element on the virtual canvas and updates virtual size of canvas
    ;; e is the new element and must be the new tail of the element list
    ;; previous is the previous tail of the element list
    (define (place-element e previous)
      (define-values (vw vh) (get-virtual-size))
      ;; calculate the current scrollbar positions in 0.0-1.0 range for init-auto-scrollbars
      (define-values (hscroll vscroll)
        (let-values ([(x y) (get-view-start)])
          (values (/ x vw)
                  (/ y vh))))
      (define snip-w (box 0))
      (define snip-h (box 0))
      (define snip-descent (box 0))
      (define snip-space (box 0))
      
      ;; current drawing position
      (define-values (x y) (calc-drawing-position previous))

      (send (element-snip e) get-extent dc x y snip-w snip-h snip-descent snip-space #f #f)
      ;(printf "snip size = ~a,~a ~ax~a ~a ~a~n" x y snip-w snip-h snip-descent snip-space)
      
      ;; coordinates for element bounding region
      (define-values (x1 y1 x2 y2) (values x y (inexact->exact (+ x (unbox snip-w))) 0))

      ;; add 1 pixel to line height to match what the standard editor canvas appears to do
      (set! y (add1 (inexact->exact (+ y (unbox snip-h)))))
      (set! y2 y)

      ;; just a list with a single element for now
      (set-element-bb-list! e (list (bounding-box x1 y1 x2 y2)))
      
      (when (> x2 vw)
        (set! canvas-width x2))
      
      (when (or (> canvas-width vw)
                (> y vh))
        ;(printf "place-element virtual size: ~ax~a to ~ax~a~n" vw vh canvas-width y)
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

      (define current-style #f)

      (send dc suspend-flush)

      (dynamic-wind
        void
        (lambda ()
          ;; only draw visible elements
          (for ([e (in-dlist visible-elements)])
            ;; assume only one bounding box until we implement word wrap
            (define bb (car (element-bb-list e)))
            ;; set the style if it has changed
            (when (not (eq? (send (element-snip e) get-style) current-style))
              (set! current-style (send (element-snip e) get-style))
              (send current-style switch-to dc #f))
            (send (element-snip e)
                  draw dc
                  (bounding-box-left bb) (bounding-box-top bb)
                  (bounding-box-left bb) (bounding-box-top bb)
                  (bounding-box-right bb) (bounding-box-bottom bb)
                  0 0 'no-caret)))
        (lambda ()
          (send dc resume-flush))))

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
                       (> (element-bottom (dlist-tail-value elements)) ch)))

    ;;
    (define/public (append-snip s [alignment 'left])
      (define e (element s alignment))
      (place-element e (dlist-tail-value elements))
      (dlist-append! elements e))
    
    ;; append string using the default stlye
    (define/public (append-string s [alignment 'left])
      (case mode
        [(plaintext)
         ;; for plaintext mode, insert each line in string as an element
         (define p (open-input-string s))
         (for ([line (in-lines p)])
           (define ss (make-object string-snip% line))
           (send ss set-style default-style)
           (define e (element ss alignment))
           (place-element e (dlist-tail-value elements))
           (dlist-append! elements e))]
        [else
         (define e (element (make-object string-snip% s) alignment))
         (place-element e (dlist-tail-value elements))
         (dlist-append! elements e)]))
      
    (define/public (get-style-list) styles)

    (define/public (set-default-stlye style-name)
      (define style (send styles find-named-style style-name))
      (if style
          (set! default-style style)
          #f))
    
    ))

(define canvas
  (new layout-canvas% (parent frame)
       (style '(hscroll vscroll resize-corner))
       ))

(define (init-styles style-list)
  (define standard (send style-list find-named-style "Standard"))
  (define standard-delta (make-object style-delta%))
  (send* standard-delta
    (set-family 'modern)
    ;(set-face font-name)
    (set-delta 'change-size 12)
    (set-delta-foreground text-fg-color)
    (set-delta-background canvas-bg-color))
  (send standard set-delta standard-delta)

  (define (make-color-style name color)
    ;; Each style created with this procedure copies "Standard" style
    ;; and creates a new style by name 'name' and with the foreground
    ;; color 'color'.
    (send (send style-list new-named-style name standard)
          set-delta (send* (make-object style-delta%)
                      (copy standard-delta)
                      (set-delta-foreground color))))

  (define (make-header-style name size)
    ;; Each style created with this procedure copies "Standard" style
    ;; and creates a new style by name 'name' and with the size 'size'.
    (send (send style-list new-named-style name standard)
          set-delta (send* (make-object style-delta%)
                      (copy standard-delta)
                      (set-delta 'change-weight 'bold)
                      (set-delta 'change-size size))))
  
  (make-color-style "Link" link-color)
  (make-color-style "Link Highlight" link-highlight-color)
  (make-header-style "Header1" 24)
  (make-header-style "Header2" 18)
  (make-header-style "Header3" 14)

  ;; create default html style
  (define html-standard (send style-list new-named-style "Html Standard" standard))
  (define html-standard-delta (make-object style-delta%))
  (send* html-standard-delta
    (set-family 'roman)
    (set-delta 'change-size 12)
    (set-delta-foreground html-text-fg-color)
    (set-delta-background html-text-bg-color))
  (send html-standard set-delta html-standard-delta)
)

(init-styles (send canvas get-style-list))
(send canvas set-canvas-background canvas-bg-color)

(send frame show #t)

(define highlander-text "He is immortal. Born in the highlands of Scotland 400 years ago, there are others like him. Some good, some evil. For centuries he has battled the forces of darkness with holy ground his only refuge. He cannot die unless you take his head and with it his power. There can be only one. He is Duncan Macleod, the Highlander!\n")
(define test-selector "gamefaqs-archive/ps2/final-fantasy-xii/FAQ_Walkthrough-by--Berserker.txt")
;(define test-selector ".")

;(send canvas append-string highlander-text)
;(send canvas append-string "\n\n")
;(send canvas append-string "text\nwith lots\nof\nnewlines")
(let ([response (gopher-fetch "gopher.endangeredsoft.org" test-selector #\0 70)])
  (send canvas append-string (port->string (gopher-response-data-port response))))
(printf "append finished~n")