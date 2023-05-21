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
             refresh
             get-canvas-background)

    ;; tentative mode options: 'plaintext, 'wrapped, 'layout, 'columns
    (define mode 'plaintext)

    ;; 
    (define xmargin horiz-margin)
    (define ymargin vert-margin)
    (define snip-xmargin 3)
    
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

    (struct word
      (str width to-next)
      #:prefab)
    
    ;; an element is a snip with a horizontal alignment
    ;; alignment can be 'left, 'right, 'center, or 'unaligned
    (struct element
      ([snip #:mutable] ; for strings this will be a raw string instead of a string-snip%
       [end-of-line #:mutable]
       [alignment #:mutable]
       [xpos #:mutable #:auto] ; position of top left corner of element, #f for hidden(hiding not implemented yet)
       [ypos #:mutable #:auto]
       [text-style #:mutable #:auto] ; only used for strings since snips have their own style
       [cached-text-extent #:mutable #:auto]
       [words #:mutable #:auto])
      #:prefab #:auto-value #f)

    ;; list of all elements in order of insertion
    (define elements (dlist-new))

    (define visible-elements #f)

    (define styles (new style-list%))
    (send styles new-named-style "Standard" (send styles find-named-style "Basic"))

    ;; style to use if no style is specified
    ;; this can be changed with set-default-style before appending a string to set its snip to this style
    (define default-style (send styles find-named-style "Standard"))
    
    (define (get-style e)
      (or (element-text-style e)
          (send (element-snip e) get-style)))

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

    (define (get-element-width e)
      (define w (box 0))
      (get-extent e dc w)
      (unbox w))
    
    (define (get-element-height e)
      (define h (box 0))
      (get-extent e dc #f h)
      (unbox h))
    
    (define (wrap-text?)
      (or (equal? mode 'wrapped)
          (equal? mode 'layout)))

    (define (draw-wrapped-text e dc x y left top right bottom)
      (define font (send (get-style e) get-font))
      (define-values (width height descent space) (send dc get-text-extent "a" font)) ; only need height, so string doesn't matter
      (define xpos x)
      (define ypos y)
      (for ([w (in-list (element-words e))])
        (if (<= (+ xpos (word-width w)) right)
            (begin 
              (send dc draw-text (word-str w) xpos ypos)
              (set! xpos (+ xpos (word-to-next w))))
            (begin
              (set! xpos left)
              (set! ypos (+ ypos height 1))
              (send dc draw-text (word-str w) xpos ypos)
              (set! xpos (+ xpos (word-to-next w)))))
        (when (>= xpos right)
          (set! xpos left)
          (set! ypos (+ ypos height 1)))))
    
    (define (draw e dc x y left top right bottom dx dy)
      (if (string? (element-snip e))
          (if (wrap-text?)
              (draw-wrapped-text e dc x y left top right bottom)
              (send dc draw-text (element-snip e) x y))
          (send (element-snip e) draw dc x y left top right bottom dx dy 'no-caret)))

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

    ;; update the manual scrollbars range and hide/unhide them
    ;; new width and height are in pixels
    (define (update-scrollbars new-width new-height)
      (define-values (cw ch) (get-client-size))
      (set-scroll-range 'horizontal (max 0 (- new-width cw)))
      (set-scroll-range 'vertical (max 0 (- new-height ch)))
      (set-scroll-page 'horizontal cw)
      (set-scroll-page 'vertical ch)
      (show-scrollbars (> new-width cw) (> new-height ch)))

    (define (calc-word-extents e)
      (define (line-break? c)
        (or (char=? c #\space)
            (char=? c #\newline)))
      
      (define (next-break-pos s pos)
        (for/last ([c (in-string s pos)]
                   [i (in-naturals pos)]
                   #:final (line-break? c))
          ;(printf "c[~a]=~a~n" i c)
          (if (line-break? c)
              i
              #f)))
      
      (define (next-word-pos s pos)
        (for/last ([c (in-string s pos)]
                   [i (in-naturals pos)]
                   #:final (not (line-break? c)))
          (if (not (line-break? c))
              i
              #f)))
      
      (define font (send (get-style e) get-font))
      (define text (element-snip e))
      (let loop ([word-start 0]
                 [words '()])
        (define end-pos (next-break-pos text word-start))
        (cond
          [end-pos
           (define s (substring text word-start end-pos))
           (define next-pos (next-word-pos text end-pos))
           (define-values (ww wh wd ws) (send dc get-text-extent s font))
           (cond
             [next-pos
              (define-values (nw nh nd ns) (send dc get-text-extent (substring text word-start next-pos) font))
              (loop next-pos
                    (cons (word s ww nw) words))]
             [else
              ; calculate next position using all the remaining text in the string, which will account
              ; for whitespace at the end of the string
              (define-values (nw nh nd ns) (send dc get-text-extent (substring text word-start) font))
              (set-element-words! e (reverse (cons (word s ww nw) words)))])]
          [else
           (define s (substring text word-start))
           (define-values (ww wh wd ws) (send dc get-text-extent s font))
           (set-element-words! e (reverse (cons (word s ww ww) words)))])))

    ;; state that needs to be maintained while adding elements in layout mode
    (define layout-left-elements '())
    (define layout-right-elements '())
    (define layout-unaligned-elements '())
    (define layout-left-width 0)
    (define layout-right-width 0)
    (define layout-unaligned-width 0)
    (define layout-baseline-pos 0)
    
    ;; calculate the horizontal space available for placing an element, i.e. the left and right bounds
    ;; vis-elements is a dlist of the relevant elements that could affect the span
    ;; y is the y position the span should be calculated at
    (define (horizontal-span vis-elements y x0 x1)
      (define left x0)
      (define right x1)
      (for ([e (in-dlist vis-elements)])
        (define xpos (element-xpos e))
        (if (<= xpos left)
            ; check left edge
            (let ([w (box 0)])
              (get-extent e dc xpos (element-ypos e) w)
              (when (> (+ xpos (unbox w)) left)
                (set! left (+ xpos (unbox w)))))
            ; check right edge
            (when (< xpos right)
              (set! right xpos)))
      (values left right)))

    (define (adjust-elements-ypos! elist ydelta)
      (for ([e (in-list layout-unaligned-elements)])
        (set-element-ypos! e (+ (element-ypos e) ydelta))))

    ;; pop elements from the layout alignment lists that don't extend to the new y value
    ;; stop at first element that does extend to the new y value
    (define (adjust-layout-lists! new-y)
      void)
    
    (define (layout-goto-new-line new-y)
      (set! place-x 0) ; place-x value isn't currently used in layout mode
      (set! place-y new-y)
      (adjust-layout-lists! new-y))
    
    (define (layout-element e total-width x y ew eh)
      (if (< (- total-width (+ layout-left-width layout-right-width layout-unaligned-width))
             ew)
          ; we don't have room for this element on the current line/y-position
          (begin
            (error "not implemented yet")
            (layout-element e total-width x y))
          ; we do have room
          (case (element-alignment e)
            [(left)
             (error "invalid alignment!")]
            [(right)
             (error "invalid alignment!")]
            [(center)
             (error "invalid alignment!")]
            [(unaligned)
             (printf "layout unaligned element~n")
             (if (empty? layout-unaligned-elements)
                 (begin
                   (set! layout-unaligned-elements (cons e layout-unaligned-elements))
                   (set! layout-unaligned-width (+ ew snip-xmargin))
                   (set! layout-baseline-pos (+ y eh))
                   (values layout-left-width y (+ layout-left-width ew) (+ y eh)))
                 (let ([x1 (+ layout-left-width layout-unaligned-width)]
                       [y1 y]
                       [x2 (+ layout-left-width layout-unaligned-width ew)]
                       [y2 (+ y eh)])
                   (when (> y2 layout-baseline-pos)
                     (define diff (- y2 layout-baseline-pos))
                     (adjust-elements-ypos! layout-unaligned-elements diff)
                     (set! layout-baseline-pos y2))
                   (when (< y2 layout-baseline-pos)
                     (define diff (- layout-baseline-pos y2))
                     (set! y1 (+ y1 diff))
                     (set! y2 (+ y2 diff)))
                   (set! layout-unaligned-elements (cons e layout-unaligned-elements))
                   (set! layout-unaligned-width (+ layout-unaligned-width ew snip-xmargin))
                   ; adjust y position to touch the baseline
                   (values x1 y1 x2 y2)))]
            [else
             (error "invalid alignment!")])))

    (define (reset-layout)
      (printf "resetting layout~n")
      (set! canvas-width 10)
      (set! canvas-height 10)
      (set! place-x 0)
      (set! place-y 0)
      
      (set! layout-left-elements '())
      (set! layout-right-elements '())
      (set! layout-unaligned-elements '())
      (set! layout-left-width 0)
      (set! layout-right-width 0)
      (set! layout-unaligned-width 0)
      (set! layout-baseline-pos 0)
      
      (for ([e (in-dlist elements)])
        (place-element e place-x place-y))
      (set-visible-elements!))
    
    ;; places element on the virtual canvas and updates virtual size of canvas
    ;; e is the new element and must be the new tail of the element list
    ;; x,y is position of element's upper left corner in canvas
    (define (place-element e x y)
      (define-values (dw dh) (get-drawable-size))
      (define-values (vw vh) (get-virtual-size))
      
      ;; coordinates for element bounding region
      ;; x2, y2 need to be set below
      ;; x1, y1 may be changed below (in the case of the layout mode)
      (define-values (x1 y1 x2 y2) (values x y 0 0))

      ;; cause get-extent to recalculate text extents by deleting cached value
      (set-element-cached-text-extent! e #f)
      
      ;; get extent of element
      (if (string? (element-snip e))
          ;; if snip is actually a string type
          (case mode
            [(wrapped)
             ;; calculate the extent of individual words
             ;; only have to do this once
             (when (not (element-words e))
               (calc-word-extents e))
             ;; calculate the extent of the text with word wrapping
             (define font (send (get-style e) get-font))
             (define-values (width height descent space) (send dc get-text-extent "a" font)) ; only need height, so string doesn't matter
             (define xpos x)
             (define ypos y)
             (for ([w (in-list (element-words e))])
               (if (< (+ xpos (word-to-next w)) dw)
                   (set! xpos (+ xpos (word-to-next w)))
                   (begin
                     ;; wrap to next line, but first check if w fits on the current line
                     (if (<= (+ xpos (word-width w)) dw)
                         (set! xpos 0)
                         (set! xpos (word-to-next w)))
                     (set! ypos (+ ypos height 1)))))
             ;; if more than one line, then set x bounds full width
             (if (= y ypos)
                 (set! x2 xpos)
                 (set! x2 dw))
             (set! y2 (+ ypos height 1))
             ;; set position for adding next element
             (if (element-end-of-line e)
                 (begin
                   (set! place-x 0)
                   (set! place-y y2))
                 (begin
                   (set! place-x xpos)
                   (set! place-y ypos)))
             ;(printf "element extent is ~ax~a - ~ax~a~n" x1 y1 x2 y2)
             ;(printf "next element at ~ax~a~n" place-x place-y)
             ;; not really a text extent in this case
             (set-element-cached-text-extent! e (text-extent (- x2 x1) (- y2 y1) descent space))]
            [(layout)
             void]
            [else
             (define snip-w (box 0))
             (define snip-h (box 0))
             (define snip-descent (box 0))
             (define snip-space (box 0))
             (get-extent e dc x y snip-w snip-h snip-descent snip-space #f #f)
             ;(printf "snip size = ~a,~a ~ax~a ~a ~a~n" x y snip-w snip-h snip-descent snip-space)
             (set! x2 (inexact->exact (+ x (unbox snip-w))))
             (set! y2 (add1 (inexact->exact (+ y (unbox snip-h)))))
             ;; set position for adding next element
             (if (element-end-of-line e)
                 (set!-values (place-x place-y) (values 0 y2))
                 (set!-values (place-x place-y) (values x2 y1)))])
          ;; if an actual snip%
          (let ([snip-w (box 0)]
                [snip-h (box 0)]
                [snip-descent (box 0)]
                [snip-space (box 0)])
            (get-extent e dc x y snip-w snip-h snip-descent snip-space #f #f)
            (case mode
              [(layout)
               (set!-values (x1 y1 x2 y2) (layout-element e dw x y (unbox snip-w) (unbox snip-h)))
               ; set position for adding next element
               (if (element-end-of-line e)
                   void
                   void)]
              [else
               ;(printf "snip size = ~a,~a ~ax~a ~a ~a~n" x y snip-w snip-h snip-descent snip-space)
               (set! x2 (inexact->exact (+ x (unbox snip-w))))
               (set! y2 (add1 (inexact->exact (+ y (unbox snip-h)))))
               ;; set position for adding next element
               (if (element-end-of-line e)
                   (set!-values (place-x place-y) (values 0 y2))
                   (set!-values (place-x place-y) (values x2 y1)))])))     
      ;(printf "element bb = (~a,~a) (~a,~a)~n" x1 y1 x2 y2)

      ;; set the element's position
      (set-element-xpos! e x1)
      (set-element-ypos! e y1)

      (when (> x2 canvas-width)
        (set! canvas-width x2))

      (when (> y2 canvas-height)
        (set! canvas-height y2)))
      
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
            ;(printf "snip at ~ax~a, text=~a~n" x y  (element-snip e))
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
            
      (refresh))
    
    (define/override (on-size width height)
      (define-values (cw ch) (get-client-size))
      (define-values (dw dh) (get-drawable-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left dw) (+ top dh)))

      (define-values (w h) (get-size))
      ;(printf "on-size client=~ax~a window=~ax~a canvas=~ax~a~n" cw ch w h dw dh)

      ;; reposition all elements
      (when (and (wrap-text?) (not (= cached-client-width cw)))
        (printf "on-size canvas ~ax~a " canvas-width canvas-height)
        (reset-layout)
        (printf "-> ~ax~a~n" canvas-width canvas-height))
      
      ;; update visible elements if window height changes
      (if (> ch cached-client-height)
          (adjust-visible-elements-forward! visible-elements top bottom)
          (adjust-visible-elements-back! visible-elements top bottom))
      ;(printf "on-size: # visible = ~a~n" (dlist-length visible-elements))
      (set! cached-client-width cw)
      (set! cached-client-height ch)

      ;; need to update the scroll range when the client size changes
      (call-with-values get-virtual-size update-scrollbars))

    ;;
    (define/public (set-mode m)
      (set! mode m)
      (unless (dlist-empty? elements)
        (reset-layout)
        (call-with-values get-virtual-size update-scrollbars)
        (refresh)))
    
    ;;
    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned])
      (define e (element s end-of-line alignment))
      (place-element e place-x place-y)
      (call-with-values get-virtual-size update-scrollbars)
      (dlist-append! elements e))
    
    ;; append string using the default stlye
    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned])
      (case mode
        [(plaintext wrapped)
         ;; for text modes, insert each line in string as an element
         ;; default to adding newline after each line/element
         (define p (open-input-string s))
         (for ([line (in-lines p)])
           (define e (element line end-of-line alignment))
           (set-element-text-style! e (or style default-style))
           (place-element e place-x place-y)
           (call-with-values get-virtual-size update-scrollbars)
           (dlist-append! elements e))]
        [else
         (define e (element s end-of-line alignment))
         (set-element-text-style! e (or style default-style))
         (place-element e place-x place-y)
         (call-with-values get-virtual-size update-scrollbars)
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

(define layout-test #t)
(if layout-test
    (send canvas set-mode 'layout)
    (send canvas set-mode 'wrapped))

(send frame show #t)

(define highlander-text "He is immortal. Born in the highlands of Scotland 400 years ago, there are others like him. Some good, some evil. For centuries he has battled the forces of darkness with holy ground his only refuge. He cannot die unless you take his head and with it his power. There can be only one. He is Duncan Macleod, the Highlander!\n")
(define test-selector "gamefaqs-archive/ps2/final-fantasy-xii/FAQ_Walkthrough-by--Berserker.txt")
;(define test-selector "/media/floppies.txt")
;(define test-selector ".")

(if layout-test
    (let ([crackdummies (make-object image-snip% "cracdumm.gif")]
          [smallavatar (make-object image-snip% "small_avatar.png")])
      (send canvas append-snip crackdummies)
      (send canvas append-snip smallavatar)
      (send canvas append-snip crackdummies))
    (begin
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
        (send canvas append-string (port->string (gopher-response-data-port response))))))

(printf "append finished~n")
