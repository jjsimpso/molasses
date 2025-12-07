#lang racket/gui

(require "dlist.rkt"
         "gopher.rkt"
         "config.rkt"
         "memoize.rkt"
         "layout.rkt"
         "table.rkt")

(provide layout-canvas%)

(define layout-canvas%
  (class canvas%
    (super-new
     (style '(hscroll vscroll resize-corner no-autoclear)))
    (init [horiz-margin 5]
          [vert-margin 5])
    (init-field [wheel-step 10]
                [smooth-scrolling #f]
                [smooth-scroll-steps 5]
                [snip-xmargin 0]
                [snip-ymargin 2])
    (inherit get-size
             get-client-size
             show-scrollbars
             init-manual-scrollbars
             suspend-flush
             resume-flush
             flush
             refresh
             refresh-now
             get-canvas-background)

    ;; tentative mode options: 'plaintext, 'wrapped, 'layout, 'columns
    (define mode 'plaintext)

    ;; 
    (define xmargin horiz-margin)
    (define ymargin vert-margin)
    
    ;; method name from editor-canvas%
    (define/public (horizontal-inset)
      xmargin)

    ;; method name from editor-canvas%
    (define/public (vertical-inset)
      ymargin)

    (define/public (set-horizontal-inset inset)
      (set! xmargin inset)
      (reset-layout)
      (refresh))

    ;; initially hide both scrollbars 
    (init-manual-scrollbars #f #f 10 10 0 0)

    ;(send this wheel-event-mode 'integer)

    (define styles (new style-list%))
    (send styles new-named-style "Standard" (send styles find-named-style "Basic"))

    ;; style to use if no style is specified
    ;; this can be changed with set-default-style before appending a string to set its snip to this style
    (define default-style (send styles find-named-style "Standard"))

    ;;
    (define offscreen-dc (new bitmap-dc%))

    (define/override (get-dc)
      offscreen-dc)
    
    ;; store the drawing context for efficiency
    ;(define dc (get-dc))

    (define canvas-dc (super get-dc))

    ; example of how to scale the entire canvas
    ;(send canvas-dc set-scale 2.0 2.0)
    
    ;; size of canvas's content, which doesn't include margins if they exist
    ;; equivalent to virtual size minus the margins
    (define canvas-width 10)
    (define canvas-height 10)
    
    ;; scroll position. upper left coordinates of viewable portion of the canvas
    (define scroll-x 0)
    (define scroll-y 0)

    ;; number of pixels to scroll for each unit of scrollbar movement
    (define scrollbar-vert-step-size 1)
    
    ;; needed so that we can tell if the canvas is getting bigger or smaller during on-size events
    (define cached-client-width 10)
    (define cached-client-height 10)

    ;; 
    (define layout-ctx (new-layout-context offscreen-dc default-style snip-xmargin snip-ymargin))

    (define/public (get-drawable-size)
      (define-values (cw ch) (get-client-size))
      (values (max 0 (- cw (* 2 xmargin)))
              (max 0 (- ch (* 2 ymargin)))))

    ;; replaces version from canvas% since we're using manual scrollbars
    (define/override (get-virtual-size)
      (values canvas-width canvas-height))

    ;; replaces version from canvas% since we're using manual scrollbars
    (define/override (get-view-start)
      (values scroll-x scroll-y))
        
    ;; list of all elements in order of insertion
    (define elements (dlist-new))

    (define visible-elements #f)

    (struct selection
      ([elements #:mutable]
       [head-start-pos #:mutable]
       [head-end-pos #:mutable]
       [tail-end-pos #:mutable])
      #:prefab)

    (struct selection-start (x y top bottom))
    
    (define mouse-selection #f)
    (define mouse-selection-start #f) 
    (define mouse-selection-end #f) ; false or a pair holding x,y coordinates in virtual canvas coordinates

    ;; a dlist of selections holding the results of a "find in page" text search
    (define find-in-canvas-selections (dlist-new))
    ; points to the dlink of the currently highlighted find result
    (define find-in-canvas-cur-dlink #f)

    (define (find-in-canvas-cur-sel)
      (if find-in-canvas-cur-dlink
          (dlink-value find-in-canvas-cur-dlink)
          #f))
    
    (define (selection-equal? s1 s2)
      (or (and (false? s1) (false? s2))
          (and s1 s2
               (eq? (dlist-head-value (selection-elements s1))
                    (dlist-head-value (selection-elements s2)))
               (eq? (dlist-tail-value (selection-elements s1))
                    (dlist-tail-value (selection-elements s2)))
               (equal? (selection-head-start-pos s1) (selection-head-start-pos s2))
               (equal? (selection-head-end-pos s1) (selection-head-end-pos s2))
               (equal? (selection-tail-end-pos s1) (selection-tail-end-pos s2)))))
    
    (define (highlightable-element? e)
      (string? (element-snip e)))
    
    (define (first-visible-element)
      (and visible-elements (dlist-head-value visible-elements)))
    
    (define background-bitmap #f)
    (define tiled-bg-bitmap #f)
    
    (define/public (set-background-image bm)
      (cond
        [bm
         (define-values (cw ch) (get-client-size))
         (define w (send bm get-width))
         (define h (send bm get-height))
         (define tile-rows (add1 (ceiling (/ ch h))))
         (define tile-cols (add1 (ceiling (/ cw w))))
         (define bg-bitmap (make-bitmap (* tile-cols w) (* tile-rows h)))
         (define bg-bitmap-dc (send bg-bitmap make-dc))
         (for* ([x (in-range 0 (* tile-cols w) w)]
                [y (in-range 0 (* tile-rows h) h)])
           (send bg-bitmap-dc draw-bitmap bm x y))
         (set! tiled-bg-bitmap bg-bitmap)
         (set! background-bitmap bm)]
        [else
         (set! tiled-bg-bitmap #f)
         (set! background-bitmap #f)]))

    (define (wrap-text?)
      (or (equal? mode 'wrapped)
          (equal? mode 'layout)))

    (define (drawing-entirely-within? y h top bottom)
      (and (>= y top) (<= (+ y h) bottom)))
    
    ;; left,top,right,bottom are in client coordinates and define a clipping region
    ;; any transparent text rendering outside of this clipping region will require a
    ;; background redraw before rendering text
    (define (draw-text dc s e x y top bottom transparent-mode?)
      (if (not transparent-mode?)
          (send dc draw-text s x y)
          (let ([extent (element-cached-text-extent e)])
            (when (not (drawing-entirely-within? y (text-extent-h extent) top bottom))
              #;(printf "  clear region before drawing text~n")
              (clear-rectangle dc x y (text-extent-w extent) (text-extent-h extent)))
            (send dc draw-text s x y))))

    ;; left-vc, top-vc are coordinates of upper left of screen in virtual canvas coordinates
    (define (draw-wrapped-text e dc left-vc top-vc top bottom)
      (for ([line (in-list (element-lines e))])
        ;(printf "draw-wrapped-text: (~a,~a) ~a~n" (wrapped-line-x line) (wrapped-line-y line) (substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line)))
        (send dc draw-text (substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line))
              (+ (- (wrapped-line-x line) left-vc) xmargin)
              (+ (- (wrapped-line-y line) top-vc) ymargin))))

    (define (draw-background-image dc x y w h)
      (define-values (vx vy) (get-view-start))
      (define srcx (remainder (+ vx x) (send background-bitmap get-width)))
      (define srcy (remainder (+ vy y) (send background-bitmap get-height)))
      (send dc draw-bitmap-section tiled-bg-bitmap x y srcx srcy w h))

    ;; left,top,right,bottom are in client coordinates and define a clipping region
    (define (draw e dc x y left top right bottom dx dy)
      (if (string? (element-snip e))
          (send dc draw-text (element-snip e) x y)
          (send (element-snip e) draw dc x y left top right bottom dx dy 'no-caret)))

    (define (draw-selection e sel sel-elements x y left top cw ch dc)
      (cond
        [(eq? e (dlist-head-value sel-elements))
         ;(printf "  draw head from ~a to ~a~n" (selection-head-start-pos sel) (selection-head-end-pos sel))
         (define-values (w u1 u2 u3) (send dc get-text-extent (substring (element-snip e) 0 (selection-head-start-pos sel))))
         (send dc draw-text (substring (element-snip e) (selection-head-start-pos sel) (selection-head-end-pos sel)) (+ x w) y)]
        [(eq? e (dlist-tail-value sel-elements))
         ;(printf "  draw tail from 0 to ~a~n" (selection-tail-end-pos sel))
         (send dc draw-text (substring (element-snip e) 0 (selection-tail-end-pos sel)) x y)]
        [else
         (if (and (wrap-text?) (string? (element-snip e)))
             (draw-wrapped-text e dc left top 0 0)
             (draw e dc
                   x y
                   0 0
                   (- cw xmargin) (- ch ymargin)
                   0 0))]))

    (define (make-highlight-style from-style toggle-underline)
      (define highlight-bg-color (send from-style get-foreground))
      (define highlight-fg-color (send from-style get-background))
      (define highlight-delta (send* (make-object style-delta%)
                                (set-delta-foreground highlight-fg-color)
                                (set-delta-background highlight-bg-color)))
      (when toggle-underline
        (send highlight-delta set-delta 'change-toggle-underline))
      (send styles find-or-create-style from-style highlight-delta))

    (define make-highlight-style-cached (memoize-2args make-highlight-style make-hasheq))
    
    (define (draw-highlight highlight-selection dc)
      (when highlight-selection
        (define-values (dw dh) (get-drawable-size))
        (define-values (cw ch) (get-client-size))
        ;; position of viewport in virtual canvas
        (define-values (left top) (get-view-start))
        (define-values (right bottom) (values (+ left dw) (+ top dh)))
        (define sel-elements (selection-elements highlight-selection))
        (define current-style #f)
        (for ([e (in-dlist sel-elements)]
              #:when (and (highlightable-element? e)
                          (element-visible? e top bottom)))
          (define highlight-style
            (make-highlight-style-cached (get-style e) (eq? highlight-selection (find-in-canvas-cur-sel))))
          (when (not (eq? highlight-style current-style))
            (set! current-style highlight-style)
            (send current-style switch-to dc #f))
          (define-values (x y) (values (+ (- (element-xpos e) left) xmargin)
                                       (+ (- (element-ypos e) top) ymargin)))
          ;(printf "highlight snip at ~ax~a, text=~a~n" (element-xpos e) (element-ypos e)  (element-snip e))
          (draw-selection e highlight-selection sel-elements x y left top cw ch dc))))

    (define (clear-highlight highlight-selection dc)
      (when highlight-selection
        (define-values (dw dh) (get-drawable-size))
        (define-values (cw ch) (get-client-size))
        ;; position of viewport in virtual canvas
        (define-values (left top) (get-view-start))
        (define-values (right bottom) (values (+ left dw) (+ top dh)))
        (define sel-elements (selection-elements highlight-selection))
        
        (define current-style #f)
        (for ([e (in-dlist sel-elements)]
              #:when (and (highlightable-element? e)
                          (element-visible? e top bottom)))
          (define-values (x y) (values (+ (- (element-xpos e) left) xmargin)
                                       (+ (- (element-ypos e) top) ymargin)))
          ;(printf "clear snip at ~ax~a, text=~a~n" (element-xpos e) (element-ypos e)  (element-snip e))
          (set! current-style (check-style-update dc (get-style e) current-style))
          ;; when clearing highlights, just redraw the entire element/string instead of trying to only clear
          ;; the actual highligted portion. not worth the effort once re-drawing the background became necessary
          (when tiled-bg-bitmap
            (define-values (w h u2 u3) (send dc get-text-extent (element-snip e)))
            (draw-background-image dc x y w h))
          (if (and (wrap-text?) (string? (element-snip e)))
             (draw-wrapped-text e dc left top 0 0)
             (draw e dc
                   x y
                   0 0
                   (- cw xmargin) (- ch ymargin)
                   0 0)))))
    
    (define (update-highlight highlight-selection old-selection dc)
      (unless (selection-equal? old-selection highlight-selection)
        (suspend-flush)
        (clear-highlight old-selection dc)
        (draw-highlight highlight-selection dc)
        (resume-flush)))
    
    (define (element-visible? e top bottom)
      (cond
        [(>= (element-ypos e) bottom) #f]
        [(>= (element-ypos e) top) #t]
        [else
         ;(define w (box 0))
         (define h (box 0))
         (get-extent layout-ctx e (element-xpos e) (element-ypos e) #f h)
         (if (>= (+ (element-ypos e) (unbox h)) top)
             #t
             #f)]))

    (define (scan-for-visible-elements-after-tail! cursor top bottom)
      ;; look up to ten elements after tail for any more visible elements (with intervening non-visible elements)
      (define advance-count
        (let loop ([i 1]
                   [tail (if (dlist-tail cursor)
                             (dlink-next (dlist-tail cursor))
                             #f)]
                   [adv-count 0])
          (cond
            [(> i 10) adv-count]
            [(not tail) adv-count]
            [else
             (define e (dlink-value tail))
             (if (element-visible? e top bottom)
                 (loop (add1 i) (dlink-next tail) i)
                 (loop (add1 i) (dlink-next tail) adv-count))])))
      (when (> advance-count 0)
        ;(printf "advancing visible-elements tail forward ~a places to find more visible elements~n" advance-count)
        (for ([i (in-range 0 advance-count)])
          (dlist-advance-tail! cursor))))
    
    (define/public (set-visible-elements!)
      (define-values (dw dh) (get-drawable-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left dw) (+ top dh)))

      ;(printf "elements head value ~a~n" (dlist-head-value elements))

      (when (> (dlist-length elements) 0)
        (define cursor (dlist-cursor elements))
        
        ;; find first visible element
        (for ([e (in-dlist cursor)]
              #:break (element-visible? e top bottom))
          (dlist-advance-head! cursor))
        
        (cond
          [(not (element-visible? (dlist-head-value cursor) top bottom))
           ;; no elements visible
           (set! visible-elements #f)]
          [else
           ;; set cursor's tail to element after first visible and advance tail until we reach the
           ;; last visible elment
           (set-dlist-tail! cursor (dlist-head-next cursor))
           (cond
             [(eq? mode 'layout)
              (dlist-advance-tail-while! cursor (lambda (e) (element-visible? e top bottom)) #:stop-before-false? #t)
              (scan-for-visible-elements-after-tail! cursor top bottom)]
             [else
              (dlist-advance-tail-while! cursor (lambda (e) (element-visible? e top bottom)) #:stop-before-false? #t)])
           (set! visible-elements cursor)
           #;(printf "set-visible-elements ~a to ~a~n" (dlist-head-value visible-elements) (dlist-tail-value visible-elements))])))

    (define (adjust-visible-elements-forward! cursor top bottom)
      ;; advance the tail to last visible element
      (dlist-advance-tail-while! cursor (lambda (e) (element-visible? e top bottom)) #:stop-before-false? #t)
      (when (eq? mode 'layout)
        (scan-for-visible-elements-after-tail! cursor top bottom))
      ;; advance the head while it isn't visible
      (dlist-advance-head-while! cursor (lambda (e) (not (element-visible? e top bottom))))
      void)

    (define (adjust-visible-elements-back! cursor top bottom)
      ;(printf "adjust-visible-elements-back! top=~a, bottom=~a~n" top bottom)
      (cond
        [(eq? mode 'layout)
         ;; retreat the head to first non-visible element
         (dlist-retreat-head-while! cursor (lambda (e) (element-visible? e top bottom)))
         ;; when scrolling up, there is one complicated scenario we must handle.
         ;; when elements have different horizontal alignments or when there are elements with a middle
         ;; vertical alignment, it is possible for non-visible elements to be
         ;; encountered before a visible element when moving the head back.
         (unless (element-visible? (dlist-head-value cursor) top bottom)
           ;; one incomplete solution: handle this by looking for
           ;; a gap between the top of the first visible element and each element as we move the head back.
           ;; if there is a gap greater than a few pixels, then we are likely on a line that used middle
           ;; vertical alignment, so keep searching for a visible element and stop when we find it.
           #;(define top-visible-e (dlink-value (dlist-head-next cursor)))
           #;(define top-visible-e-top (and top-visible-e (element-ypos top-visible-e)))
           #;(define (gap-between? e)
               (define e-bottom (+ (element-ypos e) (get-element-height layout-ctx e)))
               (printf "  gap between is (~a - ~a) = ~a , ~a~n" e-bottom top-visible-e-top (- e-bottom top-visible-e-top) (element-snip e))
               (< (- e-bottom top-visible-e-top) -2))
           #;(unless (> (dlist-retreat-head-while! cursor gap-between?) 0)
               ;; if we found a gap and skipped over any elements, then the head will be at a visible element
               ;; if we didn't find a gap, set head back to first visible element
               (dlist-advance-head! cursor))
           ;; still incomplete but better solution (despite being somewhat brute force):
           (define first-visible (dlist-head-next cursor))
           (when first-visible
             ;; if more than one element
             (for ([i (in-range 9 0 -1)]
                   #:break (or (false? (dlist-head-value cursor))
                               (element-visible? (dlist-head-value cursor) top bottom)))
               (dlist-retreat-head! cursor))
             (cond
               [(element-visible? (dlist-head-value cursor) top bottom)
                #;(printf "  found visible element ~a~n" (element-snip (dlist-head-value cursor)))
                ;; found a visible element, signal on-paint to redraw everything
                (set! last-paint-vy #f)
                ;; check for any consecutive visible elements just in case there's more than one
                (dlist-retreat-head-while! cursor (lambda (e) (element-visible? e top bottom)) #:stop-before-false? #t)]
               [else
                ;; no new visible elements found. still need to set head to first visible element
                (set-dlist-head! cursor first-visible)])))
         
         ;; retreat the tail until it is visible
         (dlist-retreat-tail-while! cursor (lambda (e) (not (element-visible? e top bottom))))]
        [else
         ;; retreat the head to first visible element
         (dlist-retreat-head-while! cursor (lambda (e) (element-visible? e top bottom)))
         ;; retreat the tail until it is visible
         (dlist-retreat-tail-while! cursor (lambda (e) (not (element-visible? e top bottom))))])
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
        #;(printf "update-visible-elements: # visible = ~a, first=~a~n" (dlist-length visible-elements) (element-snip (dlist-head-value visible-elements)))))

    (define hscroll-enabled? #f)
    (define vscroll-enabled? #f)

    (define/override (get-scroll-pos which)
      (cond
        [(eq? which 'vertical)
         (* (super get-scroll-pos which) scrollbar-vert-step-size)]
        [else
         (super get-scroll-pos which)]))

    (define/override (get-scroll-range which)
      (cond
        [(eq? which 'vertical)
         (* (super get-scroll-range which) scrollbar-vert-step-size)]
        [else
         (super get-scroll-range which)]))
    
    (define/override (set-scroll-pos which value)
      (cond
        [(eq? which 'vertical)
         ;(printf " set-scroll-pos: ~a, ~a(~a)~n" which value (quotient value scrollbar-vert-step-size))
         (super set-scroll-pos which (quotient value scrollbar-vert-step-size))]
        [else
         (super set-scroll-pos which value)]))

    (define/override (set-scroll-range which value)
      (cond
        [(eq? which 'vertical)
         ;(printf " set-scroll-range: ~a, ~a(~a)~n" which value (/ value scrollbar-vert-step-size))
         (super set-scroll-range which (quotient value scrollbar-vert-step-size))]
        [else
         (super set-scroll-range which value)]))

    (define/override (set-scroll-page which value)
      (cond
        [(eq? which 'vertical)
         ;(printf " set-scroll-page: ~a, ~a(~a)~n" which value (/ value scrollbar-vert-step-size))
         (super set-scroll-page which (quotient value scrollbar-vert-step-size))]
        [else
         (super set-scroll-page which value)]))

    (define (update-scrollbar-step-size new-width new-height)
      ; only update vertical step size for now
      (if (< new-height 30000)
          (set! scrollbar-vert-step-size 1)
          (set! scrollbar-vert-step-size (ceiling (/ new-height 30000))))
      #;(printf "reset layout set vert scrollbar step size to ~a~n" scrollbar-vert-step-size))

    ;; update the manual scrollbars range and hide/unhide them
    ;; new width and height are in pixels
    (define (update-scrollbars new-width new-height)
      #;(printf "update-scrollbars, thread=~a~n" (current-thread))
      (when (semaphore-try-wait? edit-lock)
        (define-values (dw dh) (get-drawable-size))
        (update-scrollbar-step-size new-width new-height)
        (set-scroll-range 'horizontal (exact-truncate (max 1 (- new-width dw))))
        (set-scroll-range 'vertical (exact-truncate (max 1 (- new-height dh))))
        (set-scroll-page 'horizontal 100)
        (set-scroll-page 'vertical (max 1 dh))
        ;; when scrollbar's are enabled, we need to recreate the offscreen bitmap because the client size changes
        ;(printf "hscroll-enabled=~a, vscroll-enabled=~a~n" hscroll-enabled? vscroll-enabled?)
        (when (not (equal? hscroll-enabled? (> new-width dw)))
          (set! hscroll-enabled? (> new-width dw))
          ;(printf " hscroll enabled changed to ~a (nw=~a, dw=~a)~n" hscroll-enabled? new-width dw)
          (send offscreen-dc set-bitmap #f)
          (show-scrollbars hscroll-enabled? vscroll-enabled?))
        (when (not (equal? vscroll-enabled? (> new-height dh)))
          (set! vscroll-enabled? (> new-height dh))
          ;(printf " vscroll enabled changed to ~a (nh=~a, dh=~a)~n" vscroll-enabled? new-height dh)
          (send offscreen-dc set-bitmap #f)
          (show-scrollbars hscroll-enabled? vscroll-enabled?)
          ;; the vertial scrollbar changes the client width and thus the layout
          ;; so if it changes we need to rerun the layout
          (reset-layout)
          (define-values (vx vy) (get-virtual-size))
          (define-values (ndw ndh) (get-drawable-size))
          (set-scroll-range 'horizontal (exact-truncate (max 1 (- vx ndw))))
          (set-scroll-range 'vertical (exact-truncate (max 1 (- vy ndh))))
          (set-scroll-page 'vertical (max 1 ndh)))
        (semaphore-post edit-lock)))

    (define (reset-layout)
      ;(printf "resetting layout, thread=~a~n" (current-thread))
      (set! canvas-width 10)
      (set! canvas-height 10)
      (set! layout-ctx (new-layout-context offscreen-dc default-style snip-xmargin snip-ymargin))
      (set! last-paint-vx #f)
      (set! last-paint-vy #f)
      
      (for ([e (in-dlist elements)])
        (place-element e (layout-context-place-x layout-ctx) (layout-context-place-y layout-ctx)))

      (update-scrollbar-step-size canvas-width canvas-height)
      
      (when (> (dlist-length elements) 0)
        (set-visible-elements!)))


    (define (handle-element-properties e)
      (define snip (element-snip e))
      (when (is-a? snip snip%)
        (for ([prop (in-list (element-properties e))])
          ;(printf "handle element property ~a~n" prop)
          (case (car prop)
            [(resizable)
             (define-values (dw dh) (get-drawable-size))
             (define space-available (- dw (layout-context-left-width layout-ctx) (unaligned-or-center-width layout-ctx) (layout-context-right-width layout-ctx)))
             ;(printf "handle resizable: dw=~a, space-available=~a, xmargin=~a~n" dw space-available xmargin)
             (define w (* space-available (/ (cdr prop) 100.0)))
             (define h (get-element-height layout-ctx e))
             (send snip resize w h)]))))
    
    ;; places element on the virtual canvas and updates virtual size of canvas
    ;; e is the new element and must be the new tail of the element list
    ;; x,y is position of element's upper left corner in canvas
    (define (place-element e x y)
      (define-values (dw dh) (get-drawable-size))
      (define-values (vw vh) (get-virtual-size))

      ;(printf "place-element at ~ax~a: ~a~n" x y (element-snip e))
      
      ;; coordinates for element bounding region
      ;; x2, y2 need to be set below
      ;; x1, y1 may be changed below (in the case of the layout mode)
      (define-values (x1 y1 x2 y2) (values x y 0 0))

      ;; cause get-extent to recalculate text extents by deleting cached value
      (set-element-cached-text-extent! e #f)
      
      (handle-element-properties e)
      
      ;; get extent of element
      (if (string? (element-snip e))
          ;; if snip is actually a string type
          (case mode
            [(wrapped)
             ;(printf "place-element ~a~n" (element-snip e))
             ;; calculate the extent of individual words
             ;; only have to do this once
             (when (not (element-words e))
               (calc-word-extents layout-ctx e))
             ;; calculate the extent of the text with word wrapping
             (define font (send (get-style e) get-font))
             (define-values (width height descent space) (send offscreen-dc get-text-extent "a" font)) ; only need height, so string doesn't matter
             (define xpos x)
             (define ypos y)
             (define lines '())
             (define start-pos 0) ; line's starting position (an index into the string)
             (for ([w (in-list (element-words e))])
               (if (< (+ xpos (word-to-next w)) dw)
                   (set! xpos (+ xpos (word-to-next w)))
                   (let ([w-end-x (+ xpos (word-width w))])
                     ;; wrap to next line, but first check if w fits on the current line
                     (if (<= w-end-x dw)
                         (begin
                           (set! lines (cons (wrapped-line start-pos (word-end-pos w) x ypos (- w-end-x x) height) lines))
                           (set! start-pos (word-end-pos w))
                           (set! xpos 0))
                         (begin
                           (set! lines (cons (wrapped-line start-pos (word-pos w) x ypos (- xpos x) height) lines))
                           (set! start-pos (word-pos w))
                           (set! xpos (word-to-next w))))
                     (set! ypos (+ ypos height 1)))))
             ;; add last line of element
             (when (< start-pos (string-length (element-snip e)))
               (define-values (last-line-width unused-h unused-d unused-s) (send offscreen-dc get-text-extent (substring (element-snip e) start-pos) font))
               (set! lines (cons (wrapped-line start-pos (string-length (element-snip e)) x ypos last-line-width height) lines)))
             ;; set the element's lines field and put the lines in order
             (set-element-lines! e (reverse lines))
             ;; if more than one line, then set x bounds full width
             (if (= y ypos)
                 (set! x2 xpos)
                 (set! x2 dw))
             (set! y2 (+ ypos height 1))
             ;; set position for adding next element
             (if (element-end-of-line e)
                 (begin
                   (set-layout-context-place-x! layout-ctx 0)
                   (set-layout-context-place-y! layout-ctx y2))
                 (begin
                   (set-layout-context-place-x! layout-ctx xpos)
                   (set-layout-context-place-y! layout-ctx ypos)))
             ;(printf "element extent is ~ax~a - ~ax~a~n" x1 y1 x2 y2)
             ;(printf "next element at ~ax~a~n" (layout-context-place-x ctx) (layout-context-place-y ctx))
             ;; not really a text extent in this case
             (set-element-cached-text-extent! e (text-extent (- x2 x1) (- y2 y1) descent space))]
            [(layout)
             ;; calculate the extent of individual words
             ;; only have to do this once
             (when (not (element-words e))
               (calc-word-extents layout-ctx e))
             (set!-values (x1 y1 x2 y2) (layout-string layout-ctx e dw y))
             (set-element-cached-text-extent! e (text-extent (- x2 x1) (- y2 y1) 0 0)) 
             ;(printf "layout placed ~a (~a,~a)-(~a,~a) left:~a, una/center:~a, right:~a~n" (element-alignment e) x1 y1 x2 y2 (layout-context-left-width layout-ctx) (unaligned-or-center-width layout-ctx) (layout-context-right-width layout-ctx))
             ;; set position for adding next element
             (when (element-end-of-line e)
                 (layout-advance-to-new-line layout-ctx y1))]
            [else
             (define snip-w (box 0))
             (define snip-h (box 0))
             (define snip-descent (box 0))
             (define snip-space (box 0))
             (get-extent layout-ctx e x y snip-w snip-h snip-descent snip-space #f #f)
             ;(printf "snip size = ~a,~a ~ax~a ~a ~a~n" x y (unbox snip-w) (unbox snip-h) (unbox snip-descent) (unbox snip-space))
             (set! x2 (exact-truncate (+ x (unbox snip-w))))
             (set! y2 (add1 (exact-truncate (+ y (unbox snip-h)))))
             ;; set position for adding next element
             (cond
               [(element-end-of-line e)
                (set-layout-context-place-x! layout-ctx 0)
                (set-layout-context-place-y! layout-ctx y2)]
               [else
                (set-layout-context-place-x! layout-ctx x2)
                (set-layout-context-place-y! layout-ctx y1)])])
          ;; if an actual snip%
          (let ([snip-w (box 0)]
                [snip-h (box 0)]
                [snip-descent (box 0)]
                [snip-space (box 0)]
                [snip-lspace (box 0)]
                [snip-rspace (box 0)])
            (get-extent layout-ctx e x y snip-w snip-h snip-descent snip-space snip-lspace snip-rspace)
            (case mode
              [(layout)
               (define valign-property (assoc 'valign (element-properties e)))
               ;(printf "valign-property = ~a~n" valign-property)
               (define snip-height (if (is-a? (element-snip e) string-snip%)
                                       (- (unbox snip-h) (unbox snip-descent))
                                       (unbox snip-h)))
               (set!-values (x1 y1 x2 y2) (layout-snip layout-ctx e dw y (unbox snip-w) snip-height
                                                       (if valign-property
                                                           (cdr valign-property)
                                                           'bottom)))
               ;(printf "layout placed ~a (~a,~a)-(~a,~a) left:~a, una:~a, right:~a~n" (element-alignment e) x1 y1 x2 y2 (layout-context-left-width layout-ctx) (layout-context-unaligned-width layout-ctx) (layout-context-right-width layout-ctx))
               ;(printf "extent: ~ax~a~n" (unbox snip-w) (unbox snip-h))
               ; layout-advance-to-new-line needs the element's position to be set, so set it early for now
               (set-element-xpos! e x1)
               (set-element-ypos! e y1)
               ; set position for adding next element
               ; should we allow left or right aligned elements be marked as end-of-line?
               (when (element-end-of-line e)
                 ;(printf "snip end of line~n")
                 (layout-advance-to-new-line layout-ctx y1))]
              [else
               ;(printf "snip size = ~a,~a ~ax~a ~a ~a~n" x y (unbox snip-w) (unbox snip-h) (unbox snip-descent) (unbox snip-space))
               (set! x2 (exact-truncate (+ x (unbox snip-w))))
               (set! y2 (add1 (exact-truncate (+ y (unbox snip-h)))))
               ;; set position for adding next element
               (cond
                 [(element-end-of-line e)
                  (set-layout-context-place-x! layout-ctx 0)
                  (set-layout-context-place-y! layout-ctx y2)]
                 [else
                  (set-layout-context-place-x! layout-ctx x2)
                  (set-layout-context-place-y! layout-ctx y1)])])))
      ;(printf "element bb = (~a,~a) (~a,~a)~n" x1 y1 x2 y2)

      ;; set the element's position
      (set-element-xpos! e x1)
      (set-element-ypos! e y1)

      (when (> x2 canvas-width)
        (set! canvas-width x2))

      (when (> y2 canvas-height)
        (set! canvas-height y2)))
      
    (define (clear-rectangle dc x y width height)
      ;(printf "clear-rectangle ~a,~a  ~ax~a~n" x y width height)
      (cond
        [tiled-bg-bitmap
         ;; draw tiled background image if set
         (draw-background-image dc x y width height)]
        [else
         (define old-brush (send dc get-brush))
         (define old-pen (send dc get-pen))
         (send dc set-pen (get-canvas-background) 1 'transparent)
         (send dc set-brush (get-canvas-background) 'solid)
         (send dc draw-rectangle x y width height)
         (send dc set-brush old-brush)
         (send dc set-pen old-pen)]))

    ;; update drawing context if style needs to change and return style
    (define (check-style-update dc new-style current-style)
      (cond
        [(not (eq? new-style current-style))
         (send new-style switch-to dc #f)
         (when tiled-bg-bitmap
           (send dc set-text-mode 'transparent))
         new-style]
        [else
         current-style]))

    ;; optimization provided to users of layout-canvas% so that they can avoid a complete refresh/on-paint
    ;; users only have access to the snips they added, not the actual elements, so the argument is a list
    ;; of snips to redraw
    (define/public (redraw-snips snip-list)
      (define-values (cw ch) (get-client-size))
      (define-values (vw vh) (get-virtual-size))
      ;; position of viewport in virtual canvas
      (define-values (left top) (get-view-start))

      (define current-style #f)
      
      (when (semaphore-try-wait? edit-lock)
        (dynamic-wind
          (lambda ()
            (send canvas-dc suspend-flush))
          (lambda ()
            (for ([snip (in-list snip-list)])
              (define e (lookup-visible-element-from-snip snip))
              (when e
                (define-values (x y) (values (+ (- (element-xpos e) left) xmargin)
                                             (+ (- (element-ypos e) top) ymargin)))
                #;(printf "  redraw snip at ~ax~a, text=~a~n" (element-xpos e) (element-ypos e)  (element-snip e))
                (set! current-style (check-style-update offscreen-dc (get-style e) current-style))
                (draw e offscreen-dc
                      x y
                      0 0
                      (- cw xmargin) (- ch ymargin)
                      0 0)))
            ;; blit offscreen bitmap to canvas
            (send canvas-dc draw-bitmap (send offscreen-dc get-bitmap) 0 0))
          (lambda ()
            (send canvas-dc resume-flush)
            (semaphore-post edit-lock)))))

    ;; cache virtual canvas position of last frame drawn
    (define last-paint-vx #f)
    (define last-paint-vy #f)

    (define clip-region (new region%))
    
    (define/override (on-paint)
      ;(printf "on-paint: enter~n") 

      (define start-time (current-inexact-milliseconds))

      ;; skip drawing if we are in an edit sequence
      ;; end-edit-sequence does a refresh, so that will pick up the drawing when done
      ;; grab edit-lock semaphore to prevent edit sequence during drawing
      (when (semaphore-try-wait? edit-lock)
        (dynamic-wind
          (lambda ()
            (send canvas-dc suspend-flush))
          (lambda ()
            (define-values (cw ch) (get-client-size))
            (define-values (dw dh) (get-drawable-size))
            (define-values (vw vh) (get-virtual-size))
            ;; position of viewport in virtual canvas
            (define-values (left top) (get-view-start))
            (define bottom (- (+ top ch) (* ymargin 2)))
            
            ;(printf "on-paint ~ax~a of ~ax~a at ~ax~a bottom=~a~n" cw ch vw vh left top bottom)

            (unless (send offscreen-dc get-bitmap)
              (send offscreen-dc set-bitmap (make-bitmap cw ch)))

            (when (not visible-elements)
              (set-visible-elements!))
            
            (define current-style #f)

            (define scroll-change (if last-paint-vy
                                      (- top last-paint-vy)
                                      ;; set to value that will trigger full redraw
                                      (add1 ch)))

            (define clip-left xmargin)
            (define clip-top ymargin)
            (define clip-right (- cw xmargin))
            (define clip-bottom (- ch ymargin))
            
            (send offscreen-dc set-clipping-region #f)

            ;(printf "on-paint scroll-change=~a~n" scroll-change)
            
            ;; render next frame onto offscreen bitmap
            ;; copy data from previous frame that is still visible and set new top and bottom
            ;; virtual coordinates for elements that need to be drawn
            (define-values (draw-top draw-bottom)
              (cond
                [(or (>= (abs scroll-change) dh)
                     (= scroll-change 0)
                     (not (equal? left last-paint-vx)))
                 ;; clear canvas and keep redraw set to all visible elements
                 ;(printf "redraw everything~n")
                 (clear-rectangle offscreen-dc 0 0 cw ch)
                 (values top bottom)]
                [(> scroll-change 0)
                 #;(printf "copy from ~a,~a to ~a,~a  ~ax~a~n" xmargin (+ ymargin scroll-change) xmargin ymargin dw (- ch ymargin scroll-change))
                 (send offscreen-dc copy xmargin (+ ymargin scroll-change) dw (- ch ymargin scroll-change) xmargin ymargin)
                 (set! clip-top (- ch ymargin scroll-change))
                 (set! clip-bottom (+ clip-top scroll-change))
                 ;(printf "clearing ~a,~a ~ax~a~n" xmargin clip-top cw scroll-change)
                 (clear-rectangle offscreen-dc clip-left clip-top cw scroll-change)
                 (send clip-region set-rectangle clip-left clip-top dw scroll-change)
                 (send offscreen-dc set-clipping-region clip-region)
                 (values (- bottom scroll-change) bottom)]
                [(< scroll-change 0)
                 (send offscreen-dc copy xmargin ymargin dw (- ch ymargin (abs scroll-change)) xmargin (+ ymargin (abs scroll-change)))
                 (set! clip-top ymargin)
                 (set! clip-bottom (+ clip-top (abs scroll-change)))
                 (clear-rectangle offscreen-dc clip-left clip-top cw (abs scroll-change))
                 (send clip-region set-rectangle clip-left clip-top dw (abs scroll-change))
                 (send offscreen-dc set-clipping-region clip-region)
                 (values top (+ top (abs scroll-change)))]
                [else
                 ;(printf "else condition~n")
                 (clear-rectangle offscreen-dc 0 0 cw ch)
                 (values top bottom)]))

            (when visible-elements
              (set! last-paint-vx left)
              (set! last-paint-vy top))

            ;; loop through any visible tables and update their cell coordinates
            ;; need to make sure this happens even if the table doesn't need to be redrawn
            (for ([e (in-dlist visible-elements)]
                  #:when (is-a? (element-snip e) table-snip%))
              (define-values (x y) (values (+ (- (element-xpos e) left) xmargin)
                                           (+ (- (element-ypos e) top) ymargin)))
              (send (element-snip e) update-cell-coords x y))

            ;(printf "drawing from ~a to ~a~n" draw-top draw-bottom)
            
            ;; only draw visible elements
            (for ([e (in-dlist visible-elements)]
                  #:when (element-visible? e draw-top draw-bottom))
              (define-values (x y) (values (+ (- (element-xpos e) left) xmargin)
                                           (+ (- (element-ypos e) top) ymargin)))
              #;(printf "  snip at ~ax~a, text=~a~n" (element-xpos e) (element-ypos e)  (element-snip e))
              #;(printf "  snip at ~ax~a, text=~a~n" x y (element-snip e))
              (set! current-style (check-style-update offscreen-dc (get-style e) current-style))
              (if (and (wrap-text?) (string? (element-snip e)))
                  (draw-wrapped-text e offscreen-dc left top clip-top clip-bottom)
                  (draw e offscreen-dc
                        x y
                        clip-left clip-top
                        clip-right clip-bottom
                        0 0)))

            ;; clear top, bottom and right margins in case they were drawn to
            ;; top is needed for cases where an element is partially above the viewable area
            (send offscreen-dc set-clipping-region #f)
            (clear-rectangle offscreen-dc 0 0 cw ymargin)
            (clear-rectangle offscreen-dc 0 (- ch ymargin) cw ymargin)
            (clear-rectangle offscreen-dc (- cw xmargin) 0 xmargin ch)
            
            ;; blit offscreen bitmap to canvas
            (send canvas-dc draw-bitmap (send offscreen-dc get-bitmap) 0 0)
            
            ;; draw mouse selection directly onto canvas
            (draw-highlight mouse-selection canvas-dc)
            (draw-find-results canvas-dc top bottom))
          (lambda ()
            (send canvas-dc resume-flush)
            (semaphore-post edit-lock)
            #;(printf "on-paint took ~a ms~n" (- (current-inexact-milliseconds) start-time))))))

    (define/override (on-scroll event)
      (define-values (dw dh) (get-drawable-size))
      (define refresh? #f)
      ;(printf "on-scroll: ~a ~a(~a)~n" (send event get-direction) (send event get-position) (get-scroll-pos (send event get-direction)))
      
      (if (eq? (send event get-direction) 'vertical)
          (let* ([top (get-scroll-pos 'vertical)]
                 [bottom (+ top dh)]
                 [change (- top scroll-y)])
            (when (not (= change 0))
              (set! refresh? #t))
            (set! scroll-y top)
            (if (not visible-elements)
                (set-visible-elements!)
                (update-visible-elements! change top bottom)))
          (let* ([left (get-scroll-pos 'horizontal)]
                 [change (- left scroll-x)])
            (when (not (= change 0))
              (set! refresh? #t))
            (set! scroll-x left)))
            
      (when refresh? (refresh)))
    
    (define/override (on-size width height)
      (define-values (cw ch) (get-client-size))
      (define-values (dw dh) (get-drawable-size))
      (define-values (left top) (get-view-start))
      (define-values (right bottom) (values (+ left dw) (+ top dh)))

      (define-values (w h) (get-size))
      (printf "on-size client=~ax~a(was ~ax~a) window=~ax~a canvas=~ax~a~n" cw ch cached-client-width cached-client-height w h dw dh)

      (when (not visible-elements)
        (set-visible-elements!))
      
      ;; reposition all elements
      (when (and (wrap-text?) (not (= cached-client-width cw)))
        ;(printf "on-size reposition, canvas ~ax~a~n" canvas-width canvas-height)
        (reset-layout)
        #;(printf "-> ~ax~a~n" canvas-width canvas-height))

      ;; update visible elements if window height changes
      (when visible-elements
        (if (> ch cached-client-height)
            (adjust-visible-elements-forward! visible-elements top bottom)
            (adjust-visible-elements-back! visible-elements top bottom)))
      ;(printf "on-size: # visible = ~a~n" (dlist-length visible-elements))
      (set! cached-client-width cw)
      (set! cached-client-height ch)

      (set-background-image background-bitmap)

      ;; reset the offscreen bitmap. on-paint will recreate on its next call
      (send offscreen-dc set-bitmap #f)
      (set! last-paint-vx #f)
      (set! last-paint-vy #f)
      
      ;; need to update the scroll range when the client size changes
      (define-values (vx vy) (get-virtual-size))
      (update-scrollbars vx vy))

    (define (clip-lines lines x y)
      (for/or ([wl (in-dlist lines)])
        (and (>= x (wrapped-line-x wl))
             (>= y (wrapped-line-y wl))
             (<= x (+ (wrapped-line-x wl) (wrapped-line-w wl)))
             (<= y (+ (wrapped-line-y wl) (wrapped-line-h wl))))))
    
    (define (select-element x y)
      (for/or ([e (in-dlist visible-elements)]
               #:when (and (>= y (element-ypos e))
                           (>= x (element-xpos e))))                             
        (define w (box 0))
        (define h (box 0))
        (get-extent layout-ctx e (element-xpos e) (element-ypos e) w h)
        (and (<= y (+ (element-ypos e) (unbox h)))
             (<= x (+ (element-xpos e) (unbox w)))
             (or (false? (element-lines e))
                 (clip-lines (element-lines e) x y))
             e)))

    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (case (send event get-key-code)
        [(wheel-up wheel-down)
         (define max-scroll (get-scroll-range 'vertical))
         (define scroll-pos scroll-y)
         (define new-scroll-pos
           (if (eq? key-code 'wheel-up)
               (max 0 (- scroll-pos wheel-step))
               (min max-scroll (+ scroll-pos wheel-step))))
         ;(printf "new scroll-y ~a, max ~a~n" new-scroll-pos max-scroll)
         (scroll-to new-scroll-pos #f)]
        [(up down)
         (define max-scroll (get-scroll-range 'vertical))
         (define scroll-pos scroll-y)
         (define line-height wheel-step)
         (define new-scroll-pos
           (if (eq? key-code 'up)
               (max 0 (- scroll-pos line-height))
               (min max-scroll (+ scroll-pos line-height))))
         ;(printf "new scroll-y ~a, max ~a~n" new-scroll-pos max-scroll)
         (scroll-to new-scroll-pos)]
        [(next)
         (define-values (dw dh) (get-drawable-size))
         (define-values (x y) (get-view-start))
         ;(printf "page down to ~a~n" (+ y dh))
         (scroll-to (+ y dh) smooth-scrolling)]
        [(prior)
         (define-values (dw dh) (get-drawable-size))
         (define-values (x y) (get-view-start))
         ;(printf "page up to ~a~n" (- y dh))
         (scroll-to (- y dh) smooth-scrolling)]
        [(home)
         (scroll-to 0 smooth-scrolling)]
        [(end)
         (define-values (vx vy) (get-virtual-size))
         (define-values (dw dh) (get-drawable-size))
         (scroll-to (- vy dh) smooth-scrolling)]))

    ;; store the element that has mouse focus. send on-goodbye-event to an element's snip if it loses focus
    (define element-with-focus #f)

    (define (check-element-enter-leave e x y event)
      (when (not (eq? e element-with-focus))
        ;; send on-goodbye-event to snips that lose mouse focus
        (when (and element-with-focus (is-a? (element-snip element-with-focus) snip%))
          (send (element-snip element-with-focus) on-goodbye-event canvas-dc x y (element-xpos element-with-focus) (element-ypos element-with-focus) event))
        ;; send enter event when a new snip gets focus
        (when (and e (is-a? (element-snip e) snip%))
          (send (element-snip e) adjust-cursor canvas-dc x y (- x (element-xpos e)) (- y (element-ypos e)) (new mouse-event% [event-type 'enter])))
        (set! element-with-focus e)))

    (define/override (on-event event)
      (case (send event get-event-type)
        [(left-down middle-down right-down motion)
         (define-values (left top) (get-view-start))
         (define-values (ex ey) (values (send event get-x)
                                        (send event get-y)))
         (define-values (x y) (values (- (+ ex left) xmargin)
                                      (- (+ ey top) ymargin)))
         (define e (select-element x y))

         ;(printf "on-event: ~ax~a ~ax~a~n" (send event get-x) (send event get-y) x y)

         (check-element-enter-leave e x y event)
         
         (when (and e (is-a? (element-snip e) snip%))
         #;(printf "on-event: pass event at ~ax~a to snip coords ~ax~a, canvas:~ax~a, element at ~ax~a~n"
                 (send event get-x) (send event get-y)
                 (+ (- (element-xpos e) left) xmargin) (+ (- (element-ypos e) top) ymargin)
                 x y
                 (element-xpos e) (element-ypos e))
           (send (element-snip e) on-event
                 canvas-dc
                 (+ (- (element-xpos e) left) xmargin)
                 (+ (- (element-ypos e) top) ymargin)
                 (+ (- (element-xpos e) left) xmargin)
                 (+ (- (element-ypos e) top) ymargin)
                 event))

         ;; handle text selection
         (define left-down? (send event get-left-down))
         (when (and (or (eq? mode 'plaintext) (eq? mode 'wrapped))
                    left-down?)
           (define-values (dw dh) (get-drawable-size))
           #;(printf "handle selection ~a/~a ~a ~a~n" left-down? (send event button-down?) (not (false? mouse-selection)) (describe-element e))
           (cond
             [(and (not mouse-selection-start) (send event button-down? 'left) mouse-selection)
              ;(printf "  unset text selection~n")
              (clear-highlight mouse-selection canvas-dc)
              (set! mouse-selection #f)
              (set! mouse-selection-start (new-selection-start e x y))
              (set! mouse-selection-end (cons (selection-start-x mouse-selection-start) (selection-start-y mouse-selection-start)))]
             [(and (not mouse-selection-start) (send event button-down? 'left))
              ;(printf "  set text selection~n")
              (clear-highlight mouse-selection canvas-dc)
              (set! mouse-selection #f)
              (set! mouse-selection-start (new-selection-start e x y))
              (set! mouse-selection-end (cons (selection-start-x mouse-selection-start) (selection-start-y mouse-selection-start)))]
             [(and left-down? (send event moving?))
              (define old-selection mouse-selection)
              (cond
                [(drag-selection-ahead? mouse-selection-start x y)
                 ;(printf "dragging ahead~n")
                 (set! mouse-selection (new-selection-from/to (dlist-cursor visible-elements)
                                                              (selection-start-x mouse-selection-start)
                                                              (selection-start-y mouse-selection-start)
                                                              x y))]
                [(drag-selection-behind? mouse-selection-start x y)
                 ;(printf "dragging behind~n")
                 (set! mouse-selection (new-selection-from/to (dlist-cursor visible-elements)
                                                              x y
                                                              (selection-start-x mouse-selection-start)
                                                              (selection-start-y mouse-selection-start)))])
              (set! mouse-selection-end (cons x y))

              (update-highlight mouse-selection old-selection canvas-dc)
              
              ;; check for scrolling
              (cond
                [(> ex dw)
                 (set! scroll-x (min (get-scroll-range 'horizontal) (+ scroll-x wheel-step)))
                 (set-scroll-pos 'horizontal scroll-x)
                 (refresh-now)]
                [(< ex 0)
                 (set! scroll-x (max 0 (- scroll-x wheel-step)))
                 (set-scroll-pos 'horizontal scroll-x)
                 (refresh-now)])
              (cond
                [(> ey dh)
                 ;; scroll down if mouse is below bottom edge
                 (scroll-to (min (get-scroll-range 'vertical) (+ scroll-y wheel-step)))]
                [(< ey 0)
                 ;; scroll up if mouse is above top edge
                 (scroll-to (max 0 (- scroll-y wheel-step)))])]
             [else
              void]))]
        [(left-up)
         (set! mouse-selection-start #f)
         (set! mouse-selection-end #f)]
        [else
         void]))

    ;; returns a dlist with one element representing the current selection
    (define (new-selection cursor selected-element)
      (let loop ([head (dlist-head-value cursor)])
        (cond
          [(eq? head selected-element)
           ;; set tail to #f so we have a single element dlist 
           (set-dlist-tail! cursor #f)
           (selection cursor 0 #f #f)]
          [(not (dlist-head-next cursor))
           ;; this shouldn't happen
           (error "selection doesn't match element!")
           #f]
          [else
           (dlist-advance-head! cursor)
           (loop (dlist-head-value cursor))])))

    (define (new-selection-start e x y)
      (cond
        [e
         (selection-start x y (element-ypos e) (+ (element-ypos e) (get-element-height layout-ctx e)))]
        [else
         (define same-line-element
           (for/first ([e (in-dlist visible-elements)]
                       #:when (>= (+ (element-ypos e) (get-element-height layout-ctx e)) y))
             e))
         (if same-line-element
             (selection-start x y (element-ypos same-line-element) (+ (element-ypos same-line-element) (get-element-height layout-ctx same-line-element)))
             (selection-start x y (add1 y) y))]))
    
    (define/public (clear-mouse-selection)
      (set! mouse-selection #f)
      (set! mouse-selection-start #f))
    
    (define (drag-selection-ahead? sel-start x y)
      (or (> y (selection-start-bottom sel-start))
          (and (>= y (selection-start-top sel-start))
               (>= x (selection-start-x sel-start)))))

    (define (drag-selection-behind? sel-start x y)
      (or (< y (selection-start-top sel-start))
          (and (>= y (selection-start-top sel-start))
               (< x (selection-start-x sel-start)))))

    (define (element-in-selection? sel e)
      (and sel e
           (for/first ([se (in-dlist sel)]
                       #:when (eq? se e))
             #t)))
    
    (define (pos-in-selection? sel x y)
      (cond
        [(false? sel) #f]
        [else
         (define sel-elements (selection-elements sel))
         (define tail-element (dlist-tail-value sel-elements))
         (define head-element (dlist-head-value sel-elements))
         (cond
           [(< y (element-ypos head-element))
            #f]
           [(> y (+ (element-ypos tail-element) (get-element-height layout-ctx tail-element)))
            #f]
           [(and (>= y (element-ypos tail-element))
                 (> x (+ (element-xpos tail-element) (get-element-width layout-ctx tail-element))))
            #f]
           [else
            #t])]))

    (define (calc-selection-positions! sel x0 y0 x1 y1)
      (define (find-word words xstart x)
        (for/fold ([xpos xstart]
                   [last-word #f]
                   #:result (if last-word
                                (word-pos last-word)
                                0))
                  ([w (in-list words)]
                   #:break (>= xpos x))
          (values (+ xpos (word-to-next w))
                  w)))
      (define (find-exact-pos str first-pos font xstart x)
        (for/fold ([xpos xstart]
                   [s (substring str 0 first-pos)]
                   #:result (max 0 (sub1 (string-length s))))
                  ([c (in-string (substring str first-pos))]
                   #:break (>= xpos x))
          (define new-string (string-append s (string c)))
          (define-values (w unused1 unused2 unused3) (send offscreen-dc get-text-extent new-string font))
          (values
           (+ xstart w)
           new-string)))
      ;; substring end argument is exclusive
      (define (find-exact-end-pos str first-pos font xstart x)
        (define end-pos (find-exact-pos str first-pos font xstart x))
        (min (add1 end-pos) (string-length str)))
      (define (find-head-start-pos head font x0 y0)
        #;(printf "find-head-start-pos: y0=~a, eypos=~a~n" y0 (element-ypos head))
        (if (< y0 (element-ypos head))
            ;; the start of the selection is on the line above, but to the right of all elements
            0
            (let ([start-word-pos (find-word (element-words head) (element-xpos head) x0)])
              (find-exact-pos (element-snip head) start-word-pos font (element-xpos head) x0))))
      
      (define cursor (selection-elements sel))
      (define head (dlist-head-value cursor))
      (define tail (dlist-tail-value cursor))
      ;; in plaintext mode we don't automatically calculate the word extents, since we don't normally need them
      ;; so check if they need to be calculated here
      (when (and head (not (element-words head)) (string? (element-snip head)))
        (calc-word-extents layout-ctx head))
      (when (and tail (not (element-words tail)) (string? (element-snip tail)))
        (calc-word-extents layout-ctx tail))
      (cond
        [(and head (element-words head) (eq? head tail))
         ;; selection has one element only
         (define font (send (get-style head) get-font))
         (cond
           [(> y1 (+ (element-ypos head) (get-element-height layout-ctx head)))
            ;; in this case the current cursor position is below the last line of text
            (set-selection-head-start-pos! sel (find-head-start-pos head font x0 y0))
            (set-selection-head-end-pos! sel (string-length (element-snip head)))]
           [else
            (define end-word-pos (find-word (element-words head) (element-xpos head) x1))
            (set-selection-head-start-pos! sel (find-head-start-pos head font x0 y0))
            (set-selection-head-end-pos! sel (find-exact-end-pos (element-snip head) end-word-pos font (element-xpos head) x1))
            #;(printf "  calc-selection-positions: head only ~a~n" (substring (element-snip head) (selection-head-start-pos sel) (selection-head-end-pos sel)))])
         (set-selection-tail-end-pos! sel #f)]
        [(and head (element-words head) tail (false? (element-words tail)))
         ;; head element is partial but tail is not a string
         (define font (send (get-style head) get-font))
         (set-selection-head-start-pos! sel (find-head-start-pos head font x0 y0))
         (set-selection-head-end-pos! sel (string-length (element-snip head)))
         (set-selection-tail-end-pos! sel #f)]
        [(and head (false? (element-words head)) tail (element-words tail))
         ;; head element is not a string, so only tail is partial
         #;(printf "  calc-selection-positions: tail only~n")
         (define font (send (get-style tail) get-font))
         (define end-word-pos (find-word (element-words tail) (element-xpos tail) x1))
         (set-selection-head-start-pos! sel #f)
         (set-selection-head-end-pos! sel #f)
         (set-selection-tail-end-pos! sel (find-exact-end-pos (element-snip tail) end-word-pos font (element-xpos tail) x1))]
        [(and head (element-words head) tail (element-words tail))
         ;; both
         #;(printf "  calc-selection-positions: head+tail~n")
         (define head-font (send (get-style head) get-font))
         (define tail-font (send (get-style tail) get-font))
         (set-selection-head-start-pos! sel (find-head-start-pos head head-font x0 y0))
         (set-selection-head-end-pos! sel (string-length (element-snip head)))
         (cond
           [(> y1 (+ (element-ypos tail) (get-element-height layout-ctx tail)))
            ;; in this case the current cursor position is below the last line of text
            (set-selection-tail-end-pos! sel (string-length (element-snip tail)))]
           [else
            (define end-word-pos (find-word (element-words tail) (element-xpos tail) x1))
            (set-selection-tail-end-pos! sel (find-exact-end-pos (element-snip tail) end-word-pos tail-font (element-xpos tail) x1))])]
        [else
         (set-selection-head-start-pos! sel #f)
         (set-selection-head-end-pos! sel #f)
         (set-selection-tail-end-pos! sel #f)]))
    
    (define (new-selection-from/to cursor x0 y0 x1 y1)
      (let ([x0 (max 1 x0)]
            [x1 (max 1 x1)])
        ;; clamp x0 to positive values
        #;(printf "new selection from ~a,~a to ~a,~a~n" x0 y0 x1 y1)
        (define head 
          (for ([e (in-dlist cursor)]
                #:break (or (> (element-ypos e) y0)
                            (and (> (+ (element-ypos e) (get-element-height layout-ctx e)) y0)
                                 (> (+ (element-xpos e) (get-element-width layout-ctx e)) x0))))
            (dlist-advance-head! cursor)))
        
        (cond
          [(dlist-empty? cursor)
           ;(printf "  nothing found1~n")
           #f]
          [(false? head)
           ;(printf "  nothing found2~n")
           #f]
          [else
           ;(printf "  first element ~a~n" (describe-element (dlist-head-value cursor)))
           (if (<= (element-ypos (dlist-head-value cursor)) y1)
               (let ([sel (selection cursor 0 #f #f)])
                 (set-dlist-tail! cursor #f)
                 ;; advance tail while it is in selection
                 (expand-selection-ahead sel x1 y1)
                 (calc-selection-positions! sel x0 y0 x1 y1)
                 sel)
               #f)])))

    (define (expand-selection-ahead sel x y)
      (when sel
        (define sel-elements (selection-elements sel))
        ;(printf "  expand ahead from ~a~n" (describe-element (dlist-tail-value sel-elements)))
        (let loop ([next-element (dlist-peek-tail-next sel-elements)])
          (unless (false? next-element)
            ;(printf "  checking ~a~n" (describe-element next-element))
            (define ex (element-xpos next-element))
            (define ey (element-ypos next-element))
            (when (or (> y (+ ey (get-element-height layout-ctx next-element)))
                      (and (>= y ey) (>= x ex)))
              (dlist-advance-tail! sel-elements)
              ;(printf "  advance tail to ~a~n" (describe-element (dlist-tail-value sel-elements))) 
              (loop (dlist-peek-tail-next sel-elements)))))))

    (define (shrink-selection-ahead sel x y)
      (when sel
        (define sel-elements (selection-elements sel))
        #;(printf " shrink ahead from ~a~n" (describe-element (dlist-tail-value sel-elements)))
        (let loop ([last-element (dlist-tail-value sel-elements)])
          (unless (false? last-element)
            #;(printf "  checking ~a~n" (describe-element last-element))
            (define ex (element-xpos last-element))
            (define ey (element-ypos last-element))
            (when (or (< y ey)
                      (and (>= y ey) (< x ex)))
              (if (eq? last-element (dlist-head-value sel-elements))
                  (begin
                    #;(printf "  set mouse selection to false~n")
                    (set! mouse-selection #f))
                  (begin
                    (dlist-retreat-tail! sel-elements)
                    #;(printf "  retreat tail to ~a~n" (describe-element (dlist-tail-value sel-elements))) 
                    (loop (dlist-tail-value sel-elements)))))))))

    (define (expand-selection-behind sel x y)
      (when sel
        (define sel-elements (selection-elements sel))
        #;(printf "  expand behind from ~a~n" (describe-element (dlist-head-value sel-elements)))
        (let loop ([prev-element (dlist-peek-head-prev sel-elements)])
          (unless (false? prev-element)
            #;(printf "  checking ~a~n" (describe-element prev-element))
            (define ex (element-xpos prev-element))
            (define ey (element-ypos prev-element))
            (when (or (< y ey)
                      (and (>= y ey)
                           (<= y (+ ey (get-element-height layout-ctx prev-element)))
                           (< x (+ ex (get-element-width layout-ctx prev-element)))))
              (dlist-retreat-head! sel-elements)
              #;(printf "  retreat head to ~a~n" (describe-element (dlist-head-value sel-elements)))
              (loop (dlist-peek-head-prev sel-elements)))))))

    (define (shrink-selection-behind sel x y)
      (when sel
        (define sel-elements (selection-elements sel))
        #;(printf "  shrink behind from ~a~n" (describe-element (dlist-head-value sel-elements)))
        (let loop ([head-element (dlist-head-value sel-elements)])
          (unless (false? head-element)
            #;(printf "  checking ~a~n" (describe-element head-element))
            (define ex (element-xpos head-element))
            (define ey (element-ypos head-element))
            (when (or (> y (+ ey (get-element-height layout-ctx head-element)))
                      (and (>= y ey) (> x (+ ex (get-element-width layout-ctx head-element)))))
              (if (eq? head-element (dlist-tail-value sel-elements))
                  (begin
                    #;(printf "  set mouse selection to false~n")
                    (set! mouse-selection #f))
                  (begin
                    (dlist-advance-head! sel-elements)
                    #;(printf "  advance head to ~a~n" (describe-element (dlist-head-value sel-elements)))
                    (loop (dlist-head-value sel-elements)))))))))
    
    (define (selection->string sel)
      (define head-element (dlist-head-value (selection-elements sel)))
      (define tail-element (dlist-tail-value (selection-elements sel)))
      (if (not sel)
          ""
          (for/fold ([s '()]
                     #:result (apply string-append (reverse s)))
                    ([e (in-dlist (selection-elements sel))]
                     #:when (string? (element-snip e)))
            (cond
              [(eq? e head-element)
               (cons (string-append (substring (element-snip e) (selection-head-start-pos sel) (selection-head-end-pos sel))
                                    (if (and (element-end-of-line e)
                                             (not (eq? head-element tail-element)))
                                        "\n"
                                        ""))
                     s)]
              [(eq? e tail-element)
               (cons (string-append (substring (element-snip e) 0 (selection-tail-end-pos sel)))
                     s)]
              [else
               (cons (string-append (element-snip e) (if (element-end-of-line e) "\n" ""))
                     s)]))))

    ;; needle is a byte string
    (define/public (find-in-canvas needle-string [match-case #f])
      (define (new-find-selection cursor start end)
        (selection (dlist (dlist-head cursor) #f) start end #f))
      
      (define (make-skip-table-horspool phrase)
        (define phrase-length (string-length phrase))
        (define skip-table (make-hasheqv))
        
        (for ([i (in-range 0 (- phrase-length 1))]
              [ch phrase])
          (hash-set! skip-table ch (- phrase-length i 1)))
        
        skip-table)

      ;(define compare? (if match-case char=? char-ci=?))
      
      ;; run boyer-moore-horspool string search
      ;; return dlist of selection structs representing matches
      (define (find-in-element cursor skip-table needle needle-length)
        (define e (dlist-head-value cursor))
        (define haystack
          (if match-case
              (element-snip e)
              (string-foldcase (element-snip e))))
        (define stop-pos (string-length haystack))
        (let loop ([pos (- needle-length 1)]
                   [hits (dlist-new)])
          (cond
            [(>= pos stop-pos) hits]
            [else
             (define ch (string-ref haystack pos))
             (define skip (hash-ref skip-table ch needle-length))
             (cond
               [(>= pos stop-pos) hits]
               [(not (char=? ch (string-ref needle (- needle-length 1)))) 
                (loop (+ pos skip) hits)]
               [else
                (let loop-match ([i (sub1 pos)]
                                 [needle-index (- needle-length 2)])
                  (cond
                    [(not (char=? (string-ref haystack i)
                                  (string-ref needle needle-index)))
                     (loop (+ pos skip) hits)]
                    [(= needle-index 0)
                     (loop (+ pos skip)
                           (dlist-add! hits (new-find-selection cursor i (+ i needle-length))))]
                    [else
                     (loop-match (sub1 i) (sub1 needle-index))]))])])))

      (define needle
        (if match-case
            needle-string
            (string-foldcase needle-string)))
      (define needle-length (string-length needle))
      (define skip-table (make-skip-table-horspool needle))

      (when (> needle-length 0)
        (define results
          (let loop ([cursor (dlist-cursor elements)]
                     [matches (dlist-new)])
            (define e (dlist-head-value cursor))
            (cond
              [(false? e) matches]
              [(highlightable-element? e)
               (define hits (find-in-element cursor skip-table needle needle-length))
               (if (dlist-advance-head! cursor)
                   (loop cursor (if (dlist-empty? hits) matches (dlist-append! matches hits)))
                   (if (dlist-empty? hits)
                       matches
                       (dlist-append! matches hits)))]
              [else
               (if (dlist-advance-head! cursor)
                   (loop cursor matches)
                   matches)])))
        (set! find-in-canvas-selections results))
      
      void)

    (define/public (find-results-length)
      (dlist-length find-in-canvas-selections))

    (define/public (find-results-clear)
      (set! find-in-canvas-selections (dlist-new))
      (set! find-in-canvas-cur-dlink #f))

    ;; returns #t if current find selection changed
    (define/public (find-results-next)
      (define (scroll-to-next-result)
        (define e (dlist-head-value (selection-elements (dlink-value find-in-canvas-cur-dlink))))
        (define-values (dw dh) (get-drawable-size))
        (define-values (_ top) (get-view-start))
        (define bottom (+ top dh))
        (unless (element-visible? e top bottom)
          (scroll-to (get-element-y e))))
      
      (cond
        [(and find-in-canvas-cur-dlink (dlink-next find-in-canvas-cur-dlink))
         (set! find-in-canvas-cur-dlink (dlink-next find-in-canvas-cur-dlink))
         (scroll-to-next-result)
         #t]
        [(and (false? find-in-canvas-cur-dlink) (not (dlist-empty? find-in-canvas-selections)))
         (set! find-in-canvas-cur-dlink (dlist-head find-in-canvas-selections))
         (scroll-to-next-result)
         #t]
        [else
         #f]))

    (define/public (find-results-prev)
      (define (scroll-to-prev-result)
        (define e (dlist-head-value (selection-elements (dlink-value find-in-canvas-cur-dlink))))
        (define-values (dw dh) (get-drawable-size))
        (define-values (_ top) (get-view-start))
        (define bottom (+ top dh))
        (unless (element-visible? e top bottom)
          (scroll-to (get-element-y e))))
      
      (cond
        [(and find-in-canvas-cur-dlink (dlink-prev find-in-canvas-cur-dlink))
         (set! find-in-canvas-cur-dlink (dlink-prev find-in-canvas-cur-dlink))
         (scroll-to-prev-result)
         #t]
        [else
         #f]))
    
    (define (draw-find-results dc top bottom)
      (for ([sel (in-dlist find-in-canvas-selections)])
        (define elements (selection-elements sel))
        (define head-element (dlist-head-value elements))
        (cond
          [(eq? head-element (dlist-tail-value elements))
           ; only one element in selection
           (when (and (< (element-ypos head-element) bottom)
                      (>= (+ (element-ypos head-element) (get-element-height layout-ctx head-element)) top))
             (draw-highlight sel dc))]
          [else
           (define tail-element (dlist-tail-value elements))
           (when (and (< (element-ypos head-element) bottom)
                      (>= (+ (element-ypos tail-element) (get-element-height layout-ctx tail-element)) top))
             (draw-highlight sel dc))])))

    (define/public (redo-layout)
      (unless (dlist-empty? elements)
        ; invalidate snip size caches in case the style was changed
        (for ([e (in-dlist elements)])
          (when (not (string? (element-snip e)))
            (send (element-snip e) size-cache-invalid)))
        (reset-layout)
        (let-values ([(x y) (get-virtual-size)])
          (update-scrollbars x y))
        (refresh)))
    
    (define/public (get-mode)
      mode)

    ;;
    (define/public (set-mode m)
      (unless (or (eq? mode m)
                  (not (member m '(plaintext wrapped layout))))
        (set! mode m)
        (redo-layout)))

    ;; clear the canvas contents
    (define/public (erase)
      (set! elements (dlist-new))
      (set! visible-elements #f)
      (set! scroll-x 0)
      (set! scroll-y 0)
      (set-scroll-pos 'horizontal 0)
      (set-scroll-pos 'vertical 0)
      (reset-layout)
      (refresh))

    (define/public (scroll-to y [smooth #f])
      (define-values (dw dh) (get-drawable-size))
      (define max-scroll (get-scroll-range 'vertical))
      (define old-scroll-pos scroll-y)
      (define new-scroll-pos
        (if (> y max-scroll)
            max-scroll
            (if (< y 0)
                0
                (exact-truncate y))))

      #;(printf "scroll-to ~a, thread=~a~n" y (current-thread))
      
      (when (not (= new-scroll-pos old-scroll-pos))
        (when smooth
          (let* ([step-size (/ (- new-scroll-pos old-scroll-pos) smooth-scroll-steps)]
                 [step (if (< step-size 0)
                           (min -1 (round step-size))
                           (max 1 (round step-size)))])
            (for ([pos (in-inclusive-range (+ old-scroll-pos step)
                                           new-scroll-pos
                                           step)])
              ;(printf "scroll-to ~a step-size=~a step=~a~n" pos step-size step)
              (set! scroll-y pos)
              (set-scroll-pos 'vertical pos)
              (if (not visible-elements)
                  (set-visible-elements!)
                  (update-visible-elements! step scroll-y (+ scroll-y dh)))
              (refresh-now)
              (sleep 0.016))))
        ;; handle both the non-smooth case and the final step of a smooth scroll if it didn't divide
        ;; evenly into integer steps
        (unless (= scroll-y new-scroll-pos)
          ;(printf "scroll-to ~a~n" new-scroll-pos)
          (set! scroll-y new-scroll-pos)
          (set-scroll-pos 'vertical new-scroll-pos)
          (if (not visible-elements)
              (set-visible-elements!)
              (update-visible-elements! (- new-scroll-pos old-scroll-pos) scroll-y (+ scroll-y dh)))
          (refresh))))

    (define/public (queue-scroll-to y [smooth #f])
      (queue-callback (lambda () (scroll-to y smooth)) #t))
    
    (define (lookup-element-from-snip s)
      (for/first ([e (in-dlist elements)]
                  #:when (eq? (element-snip e) s))
        e))

    (define (lookup-visible-element-from-snip s)
      (for/first ([e (in-dlist visible-elements)]
                  #:when (eq? (element-snip e) s))
        e))
    
    ;; layout-canvas% users don't have direct access to the elements, so they may need to
    ;; find an element's position using the snip(or string) that they added to the canvas
    (define/public (lookup-snip-position-size s)
      (define elem (lookup-element-from-snip s))
      (cond
        [(and elem (or (false? (element-lines elem))
                       (empty? (element-lines elem))))
         (values (element-xpos elem)
                 (element-ypos elem)
                 (get-element-width layout-ctx elem)
                 (get-element-height layout-ctx elem))]
        [elem
         (values (element-xpos elem)
                 (element-ypos elem)
                 (get-element-width layout-ctx (car (element-lines elem)))
                 (get-element-height layout-ctx (car (element-lines elem))))]
        [else
         (error "lookup-snip-position-size failed to find snip!")]))

    (define/public (first-visible-snip)
      (if (and visible-elements (dlist-head-value visible-elements))
          (element-snip (dlist-head-value visible-elements))
          #f))

    (define/public (find-anchor-position name)
      #;(printf "find-anchor-position: name=~a~n" name)
      (for*/first ([e (in-dlist elements)]
                   [prop (in-list (element-properties e))]
                   #:when (and (eq? (car prop) 'anchor)
                               (equal? (cdr prop) name)))
        #;(printf "find-anchor-position ~a~n" prop)
        (element-ypos e)))

    (define/public (layout-space-on-current-line)
      (define-values (dw dh) (get-drawable-size))
      (- dw (layout-context-left-width layout-ctx) (unaligned-or-center-width layout-ctx) (layout-context-right-width layout-ctx)))
    
    ;; add element e to the end of the elements dlist and update visible elements
    (define (append-element e)
      (define-values (dw dh) (get-drawable-size))
      (dlist-add! elements e)
      ; send a positive value for change argument to trigger a check to expand tail of the visible-elements list
      (update-visible-elements! 1 scroll-y (+ scroll-y dh)))
    
    ;; append a snip. snips have their own style
    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      (define e (element s end-of-line alignment properties))
      (place-element e (layout-context-place-x layout-ctx) (layout-context-place-y layout-ctx))
      (append-element e))
    
    ;; append a string using the default style and alignment (or provided style and alignment)
    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned] [properties '()])
      (case mode
        [(plaintext wrapped)
         ;; for text modes, insert each line in string as an element
         ;; default to adding newline after each line/element
         (define p (open-input-string s))
         (for ([line (in-lines p)])
           (define e (element line end-of-line alignment properties))
           (set-element-text-style! e (or style default-style))
           (place-element e (layout-context-place-x layout-ctx) (layout-context-place-y layout-ctx))
           (append-element e))]
        [else
         ;(printf "append-string: |~a|, eol:~a~n" s end-of-line)
         (define e (element s end-of-line alignment properties))
         (set-element-text-style! e (or style default-style))
         (place-element e (layout-context-place-x layout-ctx) (layout-context-place-y layout-ctx))
         (append-element e)]))

    (define (find-last-element [alignments-to-search '(unaligned center)])
      ;; default to not include left or right aligned elements as the "last" element
      (for/or ([e (in-dlist-reverse elements)]
               #:when (memq (element-alignment e) alignments-to-search))
        e))

    (define/public (last-element-eol? [alignments-to-search '(unaligned center)])
      ;; the end-of-line flag may only be relevant for unaligned or centered elements
      ;; unless text layout is desired for left or right content as well
      (define last-element (find-last-element alignments-to-search))
      (if last-element
          (element-end-of-line last-element)
          ;; true if no elements
          #t))

    (define/public (last-element-has-property? prop [alignments-to-search '(unaligned center)])
      (define last-element (find-last-element alignments-to-search))
      (if last-element
          (assoc prop (element-properties last-element))
          ;; true if no elements
          #t))

    ;; last element ends with whitespace
    (define/public (last-element-ews?)
      (define last-element (find-last-element))
      (if last-element
          (or (and (string? (element-snip last-element)) (string-suffix? (element-snip last-element) " "))
              (element-end-of-line last-element))
          #f))

    (define/public (set-last-element-eol [alignments-to-search '(unaligned center)])
      (define last-element (find-last-element alignments-to-search))
      (when (and last-element (not (element-end-of-line last-element)))
        (set-element-end-of-line! last-element #t)
        (layout-advance-to-new-line layout-ctx (element-ypos last-element))))
    
    (define/public (get-style-list) styles)

    (define/public (set-default-style style-or-name)
      (if (is-a? style-or-name style<%>)
          (begin
            (set! default-style style-or-name)
            (set-layout-context-default-style! layout-ctx style-or-name))
          (let ([style (send styles find-named-style style-or-name)])
            (if style
                (begin
                  (set! default-style style)
                  (set-layout-context-default-style! layout-ctx style))
                #f))))

    (define/public (can-do-edit-operation? op [recursive? #t])
      #;(printf "can-do-edit-operation? ~a~n" op)
      (cond
        [(eq? op 'copy) #t]
        [(eq? op 'paste) #f]
        [(eq? op 'select-all) #t]
        [else #f]))

    (define/public (do-edit-operation op [recursive? #t] [ts 0])
      (define (string-length-or-false e)
        (if (and e (string? (element-snip e)))
            (string-length (element-snip e))
            #f))
      #;(printf "do-edit-operation? ~a~n" op)
      (case op
        [(copy)
         (send the-clipboard set-clipboard-string (selection->string mouse-selection) ts)]
        [(paste) void]
        [(select-all)
         (set! mouse-selection 
               (selection (dlist-cursor elements)
                          0
                          (string-length-or-false (dlist-head-value elements))
                          (string-length-or-false (dlist-tail-value elements))))
         (set! mouse-selection-start #f)
         (set! mouse-selection-end #f)
         (draw-highlight mouse-selection canvas-dc)]
        [else void]))

    ;; begin/end edit-sequence is used to prevent redundant on-paint calls as well as prevent
    ;; updates to canvas data while drawing is active
    ;; WARNING: unlike in editor%'s, edit sequences will deadlock if nested
    (define in-edit-sequence #f)
    (define edit-lock (make-semaphore 1))
    
    (define/public (begin-edit-sequence)
      ;(printf "****** begin edit sequence~n")
      (semaphore-wait edit-lock)
      (set! in-edit-sequence #t))

    (define/public (in-edit-sequence?)
      in-edit-sequence)
    
    (define/public (end-edit-sequence)
      ;(printf "****** end edit sequence~n")
      ;; after adding elements to the canvas, we need to update scrollbars
      ;; do this here instead of inside every call to append a new element
      (define-values (vx vy) (get-virtual-size))
      (queue-callback
       (lambda ()
         (update-scrollbars vx vy)
         ;; force on-paint to do full redraw on next call
         (set! last-paint-vx #f)
         (set! last-paint-vy #f))
       #t)
      
      (if (not (semaphore-try-wait? edit-lock))
          (if in-edit-sequence
              ;; semaphore is held by previous call to begin-edit-sequence, release it
              (begin
                (set! in-edit-sequence #f)
                (semaphore-post edit-lock)
                (refresh))
              ;; semaphore must currently be held by on-paint, this case shouldn't happen unless
              ;; end-edit-sequence was called without a matching begin-edit-sequence
              (void))
          ;; we grabbed semaphore, release it and trigger error
          (begin
            (semaphore-post edit-lock)
            (error "end-edit-sequence called without matching begin-edit-sequence~n"))))))

(module+ main  
  (define frame 
    (new (class frame% (super-new))
         [label "Layout canvas test"]
         [width 800]
         [height 600]))

  (define canvas
    (new layout-canvas% (parent frame)
         (smooth-scrolling #t)
         (snip-xmargin 5)
         (horiz-margin 5)
         (vert-margin 5)))

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

  (define menu-item-snip%
    (class string-snip%
      (init-field [dir-entity #f])
      (inherit get-flags set-flags)
      (super-new)
      (set-flags (cons 'handles-all-mouse-events (get-flags)))
      ;(set-flags (cons 'handles-events (get-flags)))
      
      (define/override (on-event dc x y editorx editory e)
        (when (send e button-down? 'left)
          (printf "menu-item on-event: ~a~n" (send this get-text 0 30))))
      ))

  (define (add-gopher-menu c)
    (define standard-style
      (send (send c get-style-list) find-named-style "Standard"))
    (define link-style
      (send (send c get-style-list) find-named-style "Link"))

    (define (insert-menu-item c type-text display-text)
      (define type-snip (new string-snip%))
      (define link-snip (new menu-item-snip% (dir-entity #f)))
  
      ;; insert text for type indication
      (send type-snip set-style standard-style)
      (send type-snip insert type-text (string-length type-text))
      (send c append-snip type-snip)
      
      (send link-snip set-style link-style)
      (send link-snip insert display-text (string-length display-text)) ;(send link-snip get-count))
      (send c append-snip link-snip #t))

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

  (define layout-test #f)
  (if layout-test
      (send canvas set-mode 'layout)
      (send canvas set-mode 'plaintext))

  (send frame show #t)

  (define highlander-text "He is immortal. Born in the highlands of Scotland 400 years ago, there are others like him. Some good, some evil. For centuries he has battled the forces of darkness with holy ground his only refuge. He cannot die unless you take his head and with it his power. There can be only one. He is Duncan Macleod, the Highlander!\n")
  (define test-selector "gamefaqs-archive/ps2/final-fantasy-xii/FAQ_Walkthrough-by--Berserker.txt")
  ;(define test-selector "/media/floppies.txt")
  ;(define test-selector ".")
  (define bg (call-with-input-file "test/big.png" (lambda (in) (read-bitmap in))))

  (send canvas begin-edit-sequence)
  (if layout-test
      (let ([square (make-object image-snip% "test/square.png")]
            [square-left (make-object image-snip% "test/square-left.png")]
            [square-right (make-object image-snip% "test/square-right.png")]
            [square-center (make-object image-snip% "test/square-center.png")]
            [big (make-object image-snip% "test/big.png")]
            [bullet (make-object image-snip% "test/bulletr.gif")]
            [tall (make-object image-snip% "test/tall.png")]
            [tall-left (make-object image-snip% "test/tall-left.png")]
            [tall-center (make-object image-snip% "test/tall-center.png")]
            [tall-right (make-object image-snip% "test/tall-right.png")])

        (case layout-test
          [(text1)
           (send canvas append-snip square)
           (send canvas append-snip square-left #f 'left)
           (send canvas append-string "Testing, testing. " #f #f)
           (send canvas append-string highlander-text #f #f)
           (send canvas append-snip square)]
          
          [(text2)
           (send canvas append-string "" #f #f 'unaligned (list (cons 'anchor "top")))
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-string "Here is some left aligned text. Here is some left aligned text. Here is some left aligned text." #f #t 'left)
           (send canvas append-string "\n" #f #t 'left)
           (send canvas append-string "Here is some unaligned text. Here is some unaligned text. Here is some unaligned text." #f #t)
           (send canvas append-string "\n")
           (send canvas append-string "Here is some right aligned text. Here is some right aligned text. Here is some right aligned text." #f #t 'right)
           (send canvas append-string "\n")
           (send canvas append-string "****CENTERED****" (send (send canvas get-style-list) find-named-style "Highlight") #f 'center)
           (send canvas append-string "Here is some centered text. Here is some centered text. Here is some centered text." #f #t 'center)
           (send canvas append-string "\n")
           (send canvas append-snip square-left #f 'left)
           (send canvas append-snip square-right #f 'right)
           (send canvas append-snip square)
           (send canvas append-snip square)
           (send canvas append-snip tall #t)]
          [(text3)
           (send canvas append-string "Header First Line" (send (send canvas get-style-list) find-named-style "Header1") #t 'center)
           (send canvas append-string "Layout Test" (send (send canvas get-style-list) find-named-style "Header1") #f)
           (send canvas append-string "\n")
           (send canvas append-string "hel" #f #f)
           (send canvas append-string "lo" #f #t)]
          [(center)
           (send canvas append-string "Layout Test" (send (send canvas get-style-list) find-named-style "Header1") #t 'center)
           (send canvas append-string "Layout Test" (send (send canvas get-style-list) find-named-style "Header1") #f)
           (send canvas append-string "hel" #f #f)
           (send canvas append-string "lo" #f #t)
           (send canvas append-snip bullet #f 'center)
           (send canvas append-string "homepage " #f #f 'center)
           (send canvas append-snip bullet #f 'center)
           (send canvas append-string "links " #f #f 'center)
           (send canvas append-snip bullet #f 'center)
           (send canvas append-string "anonymity " #f #f 'center)
           (send canvas append-snip bullet #f 'center)
           (send canvas append-snip bullet #f 'center)
           (send canvas append-snip bullet #f 'center)
           (send canvas append-snip bullet #f 'center)
           (send canvas append-string "+ORC" #f #f 'center)
           (send canvas append-string "+ORC" #f #f 'center)
           (send canvas append-string "+ORC" #f #f 'center)
           (send canvas append-string "+ORC" #f #t 'center)
           (send canvas append-snip square-center #t 'center)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-snip square #t)
           (send canvas append-snip square-center #f 'center)
           (send canvas append-snip tall-center #f 'center)
           (send canvas append-snip square-center #t 'center)
           
           (send canvas append-snip square-right #f 'right)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-snip square-center #t 'center)
           (send canvas append-snip square-center #t 'center)
           (send canvas append-snip square)
           (send canvas append-snip square)]
          [(large)
           (send canvas append-snip square)
           (send canvas append-snip tall)
           (send canvas append-snip square #t)
           (send canvas append-snip big)
           
           (send canvas append-snip square-right #f 'right)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-snip square)
           (send canvas append-snip square)
           (send canvas append-snip square)
           (send canvas append-snip square-right #f 'right)
           (send canvas append-snip tall)]))
      (begin
        #;(let ([response (gopher-fetch "gopher.endangeredsoft.org" "games/9.png" #\0 70)])
          (send canvas append-snip
                (make-object image-snip%
                             (gopher-response-data-port response)
                             'unknown)
                #t))
        (send canvas set-background-image bg)
        (send canvas append-string highlander-text)
        (send canvas append-string "\n\n")
        (send canvas append-string "text\nwith lots\nof\nnewlines")
        (send canvas append-string "text ending with just carriage return\n")
        (send canvas append-string "text ending with carriage return + new line\r\n")
        (send canvas append-string "text ending with just new line\n")
        (send canvas append-string "text1" #f #f)
        (send canvas append-string "text2\n")
        (add-gopher-menu canvas)
        (let ([response (gopher-fetch "gopher.endangeredsoft.org" test-selector #\0 70)])
          (send canvas append-string (port->string (gopher-response-data-port response))))
        (send canvas find-in-canvas "he")))

  (send canvas end-edit-sequence)  
  (printf "append finished~n"))
