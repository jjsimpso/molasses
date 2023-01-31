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
    (init [horiz-margin 5]
          [vert-margin 5])
    (inherit get-dc
             get-size
             get-client-size
             show-scrollbars
             init-manual-scrollbars
             set-scroll-page
             set-scroll-pos
             set-scroll-range
             suspend-flush
             resume-flush
             flush
             get-canvas-background)

    ;; tentative mode options: 'plaintext, 'columnar, 'layout
    (define mode 'plaintext)

    ;; 
    (define xmargin horiz-margin)
    (define ymargin vert-margin)

    ;; initially hide both scrollbars 
    (init-manual-scrollbars #f #f (* xmargin 2) (* ymargin 2) 0 0)
    
    ;; store the drawing context for efficiency
    (define dc (get-dc))
    
    ;; size of canvas's content, which doesn't include margins if they exist
    ;; equivalent to virtual size minus the margins
    (define canvas-width 10)
    (define canvas-height 10)
    
    ;; scroll position. upper left coordinates of viewable portion of the canvas
    (define scroll-x 0)
    (define scroll-y 0)

    ;; needed so that we can tell if the canvas is getting bigger or smaller during on-size events
    (define cached-client-width 10)
    (define cached-client-height 10)

    ;; current position to place next element
    (define place-x 0)
    (define place-y 0)
    
    (define (get-drawable-size)
      (define-values (cw ch) (get-client-size))
      (values (- cw (* 2 xmargin))
              (- ch (* 2 ymargin))))

    ;; replaces version from canvas% since we're using manual scrollbars
    (define (get-virtual-size)
      (values (+ canvas-width (* xmargin 2))
              (+ canvas-height (* ymargin 2))))

    ;; replaces version from canvas% since we're using manual scrollbars
    (define (get-view-start)
      (values scroll-x scroll-y))

    (struct text-extent
      (w h descent space)
      #:prefab)
    
    ;; an element is a snip with a horizontal alignment
    ;; alignment can be 'left, 'right, or 'center
    (struct element
      ([snip #:mutable] ; for strings this will be a raw string instead of a string-snip%
       [end-of-line #:mutable]
       [alignment #:mutable]
       [xpos #:mutable] ; position of top left corner of element
       [ypos #:mutable]
       [text-style #:mutable #:auto] ; only used for strings since snips have their own style
       [cached-text-extent #:mutable #:auto]
       [line-breaks #:mutable #:auto])
      #:prefab #:auto-value #f)

    ;; list of all elements in order of insertion
    (define elements (dlist-new))

    (define visible-elements #f)

    (define styles (new style-list%))
    (send styles new-named-style "Standard" (send styles find-named-style "Basic"))

    ;; style to use if no style is specified
    ;; this can be changed with set-default-style before appending a string to set its snip to this style
    (define default-style (send styles find-named-style "Standard"))

    (define (get-extent e dc x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
      (if (string? (element-snip e))
          (if (element-cached-text-extent e)
              (let ([extent (element-cached-text-extent e)])
                (when w (set-box! w (text-extent-w extent)))
                (when h (set-box! h (text-extent-h extent)))
                (when descent (set-box! descent (text-extent-descent extent)))
                (when space (set-box! space (text-extent-space extent))))
              (let ([style (or (element-text-style e) default-style)])
                (define-values (tw th td ts) (send dc get-text-extent (element-snip e) (send style get-font)))
                (when w (set-box! w tw))
                (when h (set-box! h th))
                (when descent (set-box! descent td))
                (when space (set-box! space ts))
                (set-element-cached-text-extent! e (text-extent tw th td ts))))
          (send (element-snip e) get-extent dc x y w h descent space lspace rspace)))

    (define (draw e dc x y left top right bottom dx dy)
      (if (string? (element-snip e))
          (send dc draw-text (element-snip e) x y)
          (send (element-snip e) draw dc x y left top right bottom dx dy 'no-caret)))

    (define (get-style e)
      (or (element-text-style e)
          (send (element-snip e) get-style)))
    
    (define (element-visible? e top bottom)
      (cond
        [(>= (element-ypos e) bottom) #f]
        [(>= (element-ypos e) top) #t]
        [else
         ;(define w (box 0))
         (define h (box 0))
         (get-extent e dc (element-xpos e) (element-ypos e) #f h)
         (if (>= (+ (element-ypos e) (unbox h)) top)
             #t
             #f)]))
    
    (define/public (set-visible-elements!)
      (define-values (dw dh) (get-drawable-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left dw) (+ top dh)))

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
           ;(printf "adjust-visible-elements-back! loop 1~n")
           (when (dlist-retreat-head! cursor)
             (loop (dlist-head-value cursor)))]
          [else
           (unless (not (dlist-tail cursor))
             (set-dlist-head! cursor (dlist-head-next cursor)))]))
      ;; retreat the tail until it is visible
      (let loop ([e (dlist-tail-value cursor)])
        ;(printf "adjust-visible-elements-back! loop 2~n")
        (cond
          [(not e) void]
          [(element-visible? e top bottom) void]
          [else
           (when (dlist-retreat-tail! cursor)
             (loop (dlist-tail-value cursor)))]))
      void)
    
    ;;
    (define (update-visible-elements! scroll-change top bottom)
      ;(printf "update-visible-elements! change=~a, top=~a, bottom=~a~n" scroll-change top bottom)
      (when visible-elements
        (define head-element (dlist-head-value visible-elements))
        (define tail-element (dlist-tail-value visible-elements))
        
        (if (> scroll-change 0)
            (if (and tail-element (element-visible? tail-element top bottom))
                (adjust-visible-elements-forward! visible-elements top bottom)
                (set-visible-elements!))
            (if (and head-element (element-visible? head-element top bottom))
                (adjust-visible-elements-back! visible-elements top bottom)
                (set-visible-elements!)))
        #;(printf "update-visible-elements: # visible = ~a~n" (dlist-length visible-elements))))
    
    (define (update-drawing-position previous-element left top right bottom)
      (if previous-element
          (case mode
            [(plaintext)
             (if (element-end-of-line previous-element)
                 (set!-values (place-x place-y) (values 0 bottom))
                 (set!-values (place-x place-y) (values right top)))]
            [else
             (set!-values (place-x place-y) (values right top))])
          (set!-values (place-x place-y) (values 0 0))))
    
    ;; places element on the virtual canvas and updates virtual size of canvas
    ;; e is the new element and must be the new tail of the element list
    ;; previous is the previous tail of the element list
    (define (place-element e previous)
      (define-values (cw ch) (get-client-size))
      (define-values (vw vh) (get-virtual-size))
      (define snip-w (box 0))
      (define snip-h (box 0))
      (define snip-descent (box 0))
      (define snip-space (box 0))
      
      ;; current drawing position (currently simplified)
      (define-values (x y) (values (element-xpos e) (element-ypos e)))

      (get-extent e dc x y snip-w snip-h snip-descent snip-space #f #f)
      ;(printf "snip size = ~a,~a ~ax~a ~a ~a~n" x y snip-w snip-h snip-descent snip-space)
      
      ;; coordinates for element bounding region
      (define-values (x1 y1 x2 y2) (values x y (inexact->exact (+ x (unbox snip-w))) 0))

      ;; add 1 pixel to line height to match what the standard editor canvas appears to do
      (set! y (add1 (inexact->exact (+ y (unbox snip-h)))))
      (set! y2 y)

      ;(printf "element bb = (~a,~a) (~a,~a)~n" x1 y1 x2 y2)
      
      (update-drawing-position e x1 y1 x2 y2)
      
      (when (> x2 canvas-width)
        (set! canvas-width x2))

      (when (> y2 canvas-height)
        (set! canvas-height y2))
      
      (when (or (> (+ canvas-width (* xmargin 2)) vw)
                (> (+ canvas-height (* ymargin 2)) vh))
        ;(printf "place-element virtual size: ~ax~a to ~ax~a~n" vw vh canvas-width y)
        (define-values (cw ch) (get-client-size))
        (define horz-pixels (+ canvas-width (* xmargin 2)))
        (define vert-pixels (+ canvas-height (* ymargin 2)))
        (set-scroll-range 'horizontal (max 0 (- horz-pixels cw)))
        (set-scroll-range 'vertical (max 0 (- vert-pixels ch)))
        (show-scrollbars (> horz-pixels cw) (> vert-pixels ch))))

    (define (clear-rectangle x y width height)
      ;(printf "clear-rectangle ~a,~a  ~ax~a~n" x y width height)
      (define old-brush (send dc get-brush))
      (define old-pen (send dc get-pen))
      (send dc set-pen (get-canvas-background) 1 'transparent)
      (send dc set-brush (get-canvas-background) 'solid)
      (send dc draw-rectangle x y width height)
      (send dc set-brush old-brush)
      (send dc set-pen old-pen))

    (define/override (on-paint)
      (define-values (cw ch) (get-client-size))
      (define-values (vw vh) (get-virtual-size))
      ;; position of viewport in virtual canvas
      (define-values (left top) (values scroll-x scroll-y))
      (define-values (right bottom) (values (+ left cw) (+ top ch)))

      ;(printf "on-paint ~ax~a of ~ax~a at ~ax~a~n" cw ch vw vh left top)
      
      (when (not visible-elements)
        (set-visible-elements!))
      
      (define current-style #f)

      (send dc suspend-flush)

      (dynamic-wind
        void
        (lambda ()
          (clear-rectangle 0 0 cw ch)
          ;; only draw visible elements
          (for ([e (in-dlist visible-elements)])
            ;; set the style if it has changed
            (when (not (eq? (get-style e) current-style))
              (set! current-style (get-style e))
              (send current-style switch-to dc #f))
            (define-values (x y) (values (+ (- (element-xpos e) left) xmargin)
                                         (+ (- (element-ypos e) top) ymargin)))
            ;(printf "snip at ~ax~a (bb top=~a), text=~a~n" x y (bounding-box-top bb) (send (element-snip e) get-text 0 80))
            (draw e dc
                  x y
                  x y
                  (- cw xmargin) (- ch ymargin)
                  0 0))
          ;; clear bottom and right margins in case it was drawn to
          (clear-rectangle 0 (- ch ymargin) cw ymargin)
          (clear-rectangle (- cw xmargin) 0 xmargin ch))
        (lambda ()
          (send dc resume-flush))))

    (define/override (on-scroll event)
      (define-values (cw ch) (get-client-size))

      ;(printf "on-scroll: ~a ~a~n" (send event get-direction) (send event get-position))
      
      (if (eq? (send event get-direction) 'vertical)
          (let* ([top (send event get-position)]
                 [bottom (+ top ch)]
                 [change (- top scroll-y)])
            (set! scroll-y top)
            (if (not visible-elements)
                (set-visible-elements!)
                (update-visible-elements! change top bottom)))
          (set! scroll-x (send event get-position)))
            
      (on-paint))
    
    (define/override (on-size width height)
      (define-values (cw ch) (get-client-size))
      (define-values (dw dh) (get-drawable-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left dw) (+ top dh)))

      (define-values (w h) (get-size))
      ;(printf "on-size client=~ax~a window=~ax~a new=~ax~a~n" cw ch w h width height)

      ;; update visible elements if window height changes
      (if (> ch cached-client-height)
          (adjust-visible-elements-forward! visible-elements top bottom)
          (adjust-visible-elements-back! visible-elements top bottom))
      ;(printf "on-size: # visible = ~a~n" (dlist-length visible-elements))
      (set! cached-client-width cw)
      (set! cached-client-height ch)

      ;; need to update the scroll range when the client size changes
      (define horz-pixels (+ canvas-width (* xmargin 2)))
      (define vert-pixels (+ canvas-height (* ymargin 2)))
      (set-scroll-range 'horizontal (max 0 (- horz-pixels cw)))
      (set-scroll-range 'vertical (max 0 (- vert-pixels ch)))
      (set-scroll-page 'horizontal cw)
      (set-scroll-page 'vertical ch)
      (show-scrollbars (> canvas-width dw)
                       (> canvas-height dh)))

    ;;
    (define/public (append-snip s [end-of-line #f] [alignment 'left])
      (define e (element s end-of-line alignment place-x place-y))
      (place-element e (dlist-tail-value elements))
      (dlist-append! elements e))
    
    ;; append string using the default stlye
    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'left])
      (case mode
        [(plaintext)
         ;; for plaintext mode, insert each line in string as an element
         ;; default to adding newline after each line/element
         (define p (open-input-string s))
         (for ([line (in-lines p)])
           (define e (element line end-of-line alignment place-x place-y))
           (set-element-text-style! e (or style default-style))
           (place-element e (dlist-tail-value elements))
           (dlist-append! elements e))]
        [else
         (define e (element s end-of-line alignment place-x place-y))
         (set-element-text-style! e (or style default-style))
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
       (horiz-margin 5)
       (vert-margin 5)
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
  (send html-standard set-delta html-standard-delta))

(define (add-gopher-menu c)
  (define standard-style
    (send (send c get-style-list) find-named-style "Standard"))
  (define link-style
    (send (send c get-style-list) find-named-style "Link"))

  (define (insert-menu-item c type-text display-text)
    (send c append-string type-text standard-style #f)
    (send c append-string display-text link-style #t))

  (insert-menu-item c " (DIR) " "Directory 1")
  (insert-menu-item c " (DIR) " "Directory 2")
  (insert-menu-item c " (DIR) " "Directory 3")
  (send c append-string "       " #f #f)
  (send c append-string "\n")
  (send c append-string "       " #f #f)
  (send c append-string "Here is some informational text.")
  (send c append-string "       " #f #f)
  (send c append-string "There are a few text files below:")
  (send c append-string "       " #f #f)
  (send c append-string "\n")
  (insert-menu-item c "(TEXT) " "Read about Foo")
  (insert-menu-item c "(TEXT) " "Read about Bar"))


(init-styles (send canvas get-style-list))
(send canvas set-canvas-background canvas-bg-color)

(send frame show #t)

(define highlander-text "He is immortal. Born in the highlands of Scotland 400 years ago, there are others like him. Some good, some evil. For centuries he has battled the forces of darkness with holy ground his only refuge. He cannot die unless you take his head and with it his power. There can be only one. He is Duncan Macleod, the Highlander!\n")
(define test-selector "gamefaqs-archive/ps2/final-fantasy-xii/FAQ_Walkthrough-by--Berserker.txt")
;(define test-selector "/media/floppies.txt")
;(define test-selector ".")

(let ([response (gopher-fetch "gopher.endangeredsoft.org" "games/9.png" #\0 70)])
  (send canvas append-snip
        (make-object image-snip%
                     (gopher-response-data-port response)
                     'unknown)
        #t))
(send canvas append-string highlander-text)
(send canvas append-string "\n\n")
(send canvas append-string "text\nwith lots\nof\nnewlines")
(add-gopher-menu canvas)
(let ([response (gopher-fetch "gopher.endangeredsoft.org" test-selector #\0 70)])
  (send canvas append-string (port->string (gopher-response-data-port response))))
(printf "append finished~n")
