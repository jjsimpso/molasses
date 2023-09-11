#lang racket/gui

(require "dlist.rkt"
         "gopher.rkt"
         "config.rkt")

(provide layout-canvas%)

(define layout-canvas%
  (class canvas%
    (super-new
     (style '(hscroll vscroll resize-corner)))
    (init [horiz-margin 5]
          [vert-margin 5])
    (init-field [wheel-step 5])
    (inherit get-dc
             get-size
             get-client-size
             show-scrollbars
             init-manual-scrollbars
             set-scroll-page
             get-scroll-pos
             set-scroll-pos
             get-scroll-range
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
    (define snip-xmargin 5)
    (define snip-ymargin 3)
    
    ;; initially hide both scrollbars 
    (init-manual-scrollbars #f #f 10 10 0 0)

    ;(send this wheel-event-mode 'integer)
    
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
    
    (define/public (get-drawable-size)
      (define-values (cw ch) (get-client-size))
      (values (- cw (* 2 xmargin))
              (- ch (* 2 ymargin))))

    ;; replaces version from canvas% since we're using manual scrollbars
    (define/override (get-virtual-size)
      (values canvas-width canvas-height))

    ;; replaces version from canvas% since we're using manual scrollbars
    (define/override (get-view-start)
      (values scroll-x scroll-y))

    (struct text-extent
      (w h descent space)
      #:prefab)

    (struct word
      (str width to-next pos end-pos)
      #:prefab)

    (struct wrapped-line
      (start-pos
       end-pos
       [x #:mutable]
       [y #:mutable]
       w
       h)
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
       [words #:mutable #:auto]
       [lines #:mutable #:auto]) ; list of wrapped-line's
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
      (if (wrapped-line? e)
          (wrapped-line-w e)
          (let ([w (box 0)])
            (get-extent e dc (element-xpos e) (element-ypos e) w)
            (unbox w))))
    
    (define (get-element-height e)
      (if (wrapped-line? e)
          (wrapped-line-h e)
          (let ([h (box 0)])
            (get-extent e dc (element-xpos e) (element-ypos e) #f h)
            (unbox h))))

    (define (get-element-x e)
      (if (wrapped-line? e)
          (wrapped-line-x e)
          (element-xpos e)))

    (define (get-element-y e)
      (if (wrapped-line? e)
          (wrapped-line-y e)
          (element-ypos e)))
    
    (define (wrap-text?)
      (or (equal? mode 'wrapped)
          (equal? mode 'layout)))

    (define (draw-wrapped-text e dc left top)
      (for ([line (in-list (element-lines e))])
        ;(printf "draw-wrapped-text: (~a,~a) ~a~n" (wrapped-line-x line) (wrapped-line-y line) (substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line)))
        (send/apply dc draw-text `(,(substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line)) ,(+ (- (wrapped-line-x line) left) xmargin) ,(+ (- (wrapped-line-y line) top) ymargin)))))
    
    (define (draw e dc x y left top right bottom dx dy)
      (if (string? (element-snip e))
          (send dc draw-text (element-snip e) x y)
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

      ;(printf "elements head value ~a~n" (dlist-head-value elements))

      (when (> (dlist-length elements) 0)
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
        #;(printf "set-visible-elements ~a to ~a~n" (dlist-head-value visible-elements) (dlist-tail-value visible-elements))))

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
      (define-values (dw dh) (get-drawable-size))
      (set-scroll-range 'horizontal (max 0 (- new-width dw)))
      (set-scroll-range 'vertical (max 0 (- new-height dh)))
      (set-scroll-page 'horizontal dw)
      (set-scroll-page 'vertical dh)
      (show-scrollbars (> new-width dw) (> new-height dh)))

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
      (define last-pos (string-length text))
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
                    (cons (word s ww nw word-start next-pos) words))]
             [else
              ; calculate next position using all the remaining text in the string, which will account
              ; for whitespace at the end of the string
              (define-values (nw nh nd ns) (send dc get-text-extent (substring text word-start) font))
              (set-element-words! e (reverse (cons (word s ww nw word-start last-pos) words)))])]
          [else
           (define s (substring text word-start))
           (define-values (ww wh wd ws) (send dc get-text-extent s font))
           (set-element-words! e (reverse (cons (word s ww ww word-start last-pos) words)))])))

    ;; state that needs to be maintained while adding elements in layout mode
    (define layout-left-elements '())
    (define layout-right-elements '())
    (define layout-unaligned-elements '())
    (define layout-center-elements '())
    (define layout-left-width 0)
    (define layout-right-width 0)
    (define layout-unaligned-width 0)
    (define layout-center-width 0)
    (define layout-baseline-pos 0)

    (define (adjust-elements-xpos! elist xdelta)
      (for ([e (in-list elist)])
        (if (wrapped-line? e)
            (set-wrapped-line-x! e (+ (wrapped-line-x e) xdelta))
            (set-element-xpos! e (+ (element-xpos e) xdelta)))))
    
    (define (adjust-elements-ypos! elist ydelta)
      (for ([e (in-list elist)])
        (if (wrapped-line? e)
            (set-wrapped-line-y! e (+ (wrapped-line-y e) ydelta))
            (set-element-ypos! e (+ (element-ypos e) ydelta)))))

    ; add contract checking ll for not empty
    (define (highest-element ll)
      (define highest-y (get-element-y (car ll)))
      (define highest-e (car ll))
      (for ([e (in-list ll)])
        (when (< (get-element-y e) highest-y)
          (set! highest-y (get-element-y e))
          (set! highest-e e)))
      highest-e)
    
    ; calculate the amount that unaligned elements can be shifted to the right
    ; this can be different than the available width because there may not be a right-aligned
    ; element on the current line but one further up could still block shifting
    (define (unaligned-right-wiggle-room total-width)
      (if (empty? layout-unaligned-elements)
          (- total-width layout-right-width layout-left-width)
          (let* ([hue (highest-element layout-unaligned-elements)]
                 [ystart (get-element-y hue)]
                 [yend (+ ystart (get-element-height hue))])
            (define uedge (car layout-unaligned-elements))
            (define xend (+ (get-element-x uedge) (get-element-width uedge) snip-xmargin))
            (define wiggle-room (- total-width xend))
            (for ([e (in-dlist elements)]
                  #:when (and (eq? (element-alignment e) 'right)
                              (<= (get-element-y e) yend)
                              (>= (+ (get-element-y e) (get-element-height e)) ystart)))
              (define space-on-right (- (get-element-x e) xend))
              (when (< space-on-right wiggle-room)
                (set! wiggle-room space-on-right)))
            (printf "wiggle room = ~a~n" wiggle-room)
            wiggle-room)))
    
    ;; pop elements from the list of elements 'll' that don't extend to the new y value
    ;; 'lwidth' is the width of the items in 'll'
    ;; stop at first element that does extend to the new y value
    ;; returns the new list and the width in pixels of the items in that list
    (define (pop-layout-list ll lwidth new-y)
      ;(printf "pop-layout-list~n")
      (if (empty? ll)
          (values ll lwidth)
          (let ([e (car ll)])
            (let ([w (get-element-width e)]
                  [h (get-element-height e)])
              (if (< (+ (get-element-y e) h)
                     new-y)
                  (pop-layout-list (cdr ll) (- lwidth w snip-xmargin) new-y)
                  (values ll lwidth))))))

    (define (next-line-y-pos original)
      (define (bottom-edge-of-elements ll)
        (if (not (empty? ll))
            (+ (get-element-y (car ll))
               (get-element-height (car ll)))
            (error "empty list")))

      (add1
       (cond
         [(not (empty? layout-unaligned-elements))
          (bottom-edge-of-elements layout-unaligned-elements)]
         [(not (empty? layout-center-elements))
          (bottom-edge-of-elements layout-center-elements)]
         [(not (empty? layout-left-elements))
          (if (empty? layout-right-elements)
              (bottom-edge-of-elements layout-left-elements)
              (min (bottom-edge-of-elements layout-left-elements)
                   (bottom-edge-of-elements layout-right-elements)))]
         [(not (empty? layout-right-elements))
          (bottom-edge-of-elements layout-right-elements)]
         [else
          (printf "next-line-y-pos: all layout lists are empty~n")
          (sub1 original)])))
    
    (define (layout-goto-new-line new-y)
      (printf "layout-goto-new-line: ~a~n" new-y)
      (set! place-x 0) ; place-x value isn't currently used in layout mode
      (set! place-y new-y)
      (set! layout-baseline-pos new-y)
      (set!-values (layout-left-elements layout-left-width) (pop-layout-list layout-left-elements layout-left-width new-y))
      (set!-values (layout-right-elements layout-right-width) (pop-layout-list layout-right-elements layout-right-width new-y))
      (set!-values (layout-center-elements layout-center-width) (pop-layout-list layout-center-elements layout-center-width new-y))
      (set!-values (layout-unaligned-elements layout-unaligned-width) (pop-layout-list layout-unaligned-elements layout-unaligned-width new-y)))

    (define (layout-string e total-width y)
      ;; calculate the extent of the text with word wrapping
      (define font (send (get-style e) get-font))
      (define-values (width height descent space) (send dc get-text-extent "a" font)) ; only need height, so string doesn't matter

      (case (element-alignment e)
        [(right)
         (define end-x (- total-width layout-right-width))
         (define x end-x)
         (define left-margin (+ layout-left-width (unaligned-or-center-width)))
         (define xpos end-x)
         (define ypos y)
         (define lines '())
         (define start-pos 0) ; line's starting position (an index into the string)
         (for ([w (in-list (element-words e))])
           (if (> (- xpos (word-to-next w)) left-margin)
               (set! xpos (- xpos (word-to-next w)))
               (let ([w-start-x (- xpos (word-width w))])
                 ;; wrap to next line, but first check if w fits on the current line
                 (if (>= w-start-x left-margin)
                     (let ([line-width (- end-x w-start-x)])
                       (set! lines (cons (wrapped-line start-pos (word-end-pos w) w-start-x ypos line-width height) lines))
                       (set! start-pos (word-end-pos w))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (layout-goto-new-line ypos)
                       (set! left-margin (+ layout-left-width (unaligned-or-center-width)))
                       (set! xpos end-x)
                       (when (< w-start-x x)
                         (set! x w-start-x)))
                     (let ([line-width (- end-x xpos)])
                       (set! lines (cons (wrapped-line start-pos (word-pos w) xpos ypos line-width height) lines))
                       (set! start-pos (word-pos w))
                       (when (< xpos x)
                         (set! x xpos))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (layout-goto-new-line ypos)
                       (set! left-margin (+ layout-left-width (unaligned-or-center-width)))
                       (set! xpos (- end-x (word-to-next w))))))))
         ;; add last line of element
         (when (< start-pos (string-length (element-snip e)))
           (define-values (last-line-width unused-h unused-d unused-s) (send dc get-text-extent (substring (element-snip e) start-pos) font))
           (printf "adding last line of element (~a,~a) ~ax~a~n" xpos ypos last-line-width height)
           (set! lines (cons (wrapped-line start-pos (string-length (element-snip e)) xpos ypos last-line-width height) lines)))
         ;; add last line to layout list
         (set! layout-right-elements (cons (car lines) layout-right-elements))
         (set! layout-right-width (+ layout-right-width (- end-x xpos) snip-xmargin))
         ;; set the element's lines field and put the lines in order
         (set-element-lines! e (reverse lines))
         (values x y (- end-x x) (+ ypos height))]
        [(center)
         (define space-available (- total-width layout-left-width layout-right-width))
         (define x (+ layout-left-width (/ space-available 2)))
         (define max-width 0)
         (define width 0)
         (define ypos y)
         (define baseline (+ ypos height))
         (if (> baseline layout-baseline-pos)
             (when (not (empty? layout-center-elements))
               (define diff (- baseline layout-baseline-pos))
               (adjust-elements-ypos! layout-center-elements diff))
             (let ([diff (- layout-baseline-pos baseline)])
               (set! ypos (+ ypos diff))))
         (define lines '())
         (define start-pos 0) ; line's starting position (an index into the string)
         (for ([w (in-list (element-words e))])
           (if (< (+ width (word-to-next w)) space-available)
               (set! width (+ width (word-to-next w)))
               (let ([w-width (+ width (word-width w))])
                 ;; wrap to next line, but first check if w fits on the current line
                 (if (<=  w-width space-available)
                     (let* ([margin (/ (- space-available w-width) 2)]
                            [xpos (+ layout-left-width margin)])
                       (set! lines (cons (wrapped-line start-pos (word-end-pos w) xpos ypos w-width height) lines))
                       (set! start-pos (word-end-pos w))
                       (when (> w-width max-width)
                         (set! max-width w-width))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (set! width 0)
                       (layout-goto-new-line ypos)
                       (when (< xpos x)
                         (set! x xpos)))
                     (let* ([margin (/ (- space-available width) 2)]
                            [xpos (+ layout-left-width margin)])
                       (set! lines (cons (wrapped-line start-pos (word-pos w) xpos ypos width height) lines))
                       (set! start-pos (word-pos w))
                       (when (> width max-width)
                         (set! max-width width))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (set! width 0)
                       (layout-goto-new-line ypos)
                       (when (< xpos x)
                         (set! x xpos)))))))
         ;; add last line of element
         (when (< start-pos (string-length (element-snip e)))
           (define-values (last-line-width unused-h unused-d unused-s) (send dc get-text-extent (substring (element-snip e) start-pos) font))
           (when (> last-line-width max-width)
             (set! max-width last-line-width))
           (set! width last-line-width)
           (define margin (/ (- space-available last-line-width) 2))
           (define xpos (+ layout-left-width margin))
           (printf "adding last line of element (~a,~a) ~ax~a~n" xpos ypos last-line-width height)
           (set! lines (cons (wrapped-line start-pos (string-length (element-snip e)) xpos ypos last-line-width height) lines)))
         ;; update the baseline position after all lines are placed
         (set! layout-baseline-pos (+ ypos height))
         ;; add last line to layout list
         (set! layout-center-elements (cons (car lines) layout-center-elements))
         (set! layout-center-width (+ width snip-xmargin))
         ;; set the element's lines field and put the lines in order
         (set-element-lines! e (reverse lines))
         (values x y max-width (+ ypos height))]
        [(left)
         (define x layout-left-width)
         (define line-x x) ; starting x value of current line
         (define max-width 0)
         (define right-margin (- total-width (unaligned-or-center-width) layout-right-width))
         (define xpos x)
         (define ypos y)
         (define lines '())
         (define start-pos 0) ; line's starting position (an index into the string)
         (for ([w (in-list (element-words e))])
           (if (< (+ xpos (word-to-next w)) right-margin)
               (set! xpos (+ xpos (word-to-next w)))
               (let ([w-end-x (+ xpos (word-width w))])
                 ;; wrap to next line, but first check if w fits on the current line
                 (if (<=  w-end-x right-margin)
                     (begin
                       (set! lines (cons (wrapped-line start-pos (word-end-pos w) x ypos (- w-end-x x) height) lines))
                       (set! start-pos (word-end-pos w))
                       (when (> w-end-x max-width)
                         (set! max-width w-end-x))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (layout-goto-new-line ypos)
                       (set! right-margin (- total-width (unaligned-or-center-width) layout-right-width))
                       (set! line-x layout-left-width)
                       (set! xpos line-x)
                       (when (< line-x x)
                         (set! x line-x)))
                     (begin
                       (set! lines (cons (wrapped-line start-pos (word-pos w) x ypos (- xpos x) height) lines))
                       (set! start-pos (word-pos w))
                       (when (> xpos max-width)
                         (set! max-width xpos))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (layout-goto-new-line ypos)
                       (set! right-margin (- total-width (unaligned-or-center-width) layout-right-width))
                       (set! line-x layout-left-width)
                       (set! xpos (+ line-x (word-to-next w)))
                       (when (< line-x x)
                         (set! x line-x)))))))
         ;; add last line of element
         (when (< start-pos (string-length (element-snip e)))
           (define-values (last-line-width unused-h unused-d unused-s) (send dc get-text-extent (substring (element-snip e) start-pos) font))
           (when (> last-line-width max-width)
             (set! max-width last-line-width))
           (printf "adding last line of element (~a,~a) ~ax~a~n" line-x ypos last-line-width height)
           (set! lines (cons (wrapped-line start-pos (string-length (element-snip e)) line-x ypos last-line-width height) lines)))
         ;; add last line to layout list
         (set! layout-left-elements (cons (car lines) layout-left-elements))
         (set! layout-left-width (+ layout-left-width (- xpos line-x) snip-xmargin))
         ;; set the element's lines field and put the lines in order
         (set-element-lines! e (reverse lines))
         (values x y max-width (+ ypos height))]
        [(unaligned)
         (define x (+ layout-left-width layout-unaligned-width))
         (define line-x x) ; starting x value of current line
         (define max-width 0)
         (define right-margin (- total-width layout-right-width))
         (define xpos x)
         (define ypos y)
         (define baseline (+ ypos height))
         (if (> baseline layout-baseline-pos)
             (when (not (empty? layout-unaligned-elements))
               (define diff (- baseline layout-baseline-pos))
               (adjust-elements-ypos! layout-unaligned-elements diff))
             (let ([diff (- layout-baseline-pos baseline)])
               (set! ypos (+ ypos diff))))
         (define lines '())
         (define start-pos 0) ; line's starting position (an index into the string)
         (for ([w (in-list (element-words e))])
           (if (< (+ xpos (word-to-next w)) right-margin)
               (set! xpos (+ xpos (word-to-next w)))
               (let ([w-end-x (+ xpos (word-width w))])
                 ;; wrap to next line, but first check if w fits on the current line
                 (if (<=  w-end-x right-margin)
                     (begin
                       (set! lines (cons (wrapped-line start-pos (word-end-pos w) x ypos (- w-end-x x) height) lines))
                       (set! start-pos (word-end-pos w))
                       (when (> w-end-x max-width)
                         (set! max-width w-end-x))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (layout-goto-new-line ypos)
                       (set! right-margin (- total-width layout-right-width))
                       (set! line-x layout-left-width)
                       (set! xpos line-x)
                       (when (< line-x x)
                         (set! x line-x)))
                     (begin
                       (set! lines (cons (wrapped-line start-pos (word-pos w) x ypos (- xpos x) height) lines))
                       (set! start-pos (word-pos w))
                       (when (> xpos max-width)
                         (set! max-width xpos))
                       ;; advance to the new line
                       (set! ypos (+ ypos height 1))
                       (layout-goto-new-line ypos)
                       (set! right-margin (- total-width layout-right-width))
                       (set! line-x layout-left-width)
                       (set! xpos (+ line-x (word-to-next w)))
                       (when (< line-x x)
                         (set! x line-x)))))))
         ;; add last line of element
         (when (< start-pos (string-length (element-snip e)))
           (define-values (last-line-width unused-h unused-d unused-s) (send dc get-text-extent (substring (element-snip e) start-pos) font))
           (when (> last-line-width max-width)
             (set! max-width last-line-width))
           (printf "adding last line of element (~a,~a) ~ax~a~n" line-x ypos last-line-width height)
           (set! lines (cons (wrapped-line start-pos (string-length (element-snip e)) line-x ypos last-line-width height) lines)))
         ;; update the baseline position after all lines are placed
         (set! layout-baseline-pos (+ ypos height))
         ;; add last line to layout list
         (set! layout-unaligned-elements (cons (car lines) layout-unaligned-elements))
         (set! layout-unaligned-width (+ layout-unaligned-width (- xpos line-x) snip-xmargin))
         ;; set the element's lines field and put the lines in order
         (set-element-lines! e (reverse lines))
         (values x y max-width (+ ypos height))]))

    (define (unaligned-or-center-width)
      (cond
        [(> layout-unaligned-width 0) layout-unaligned-width]
        [(> layout-center-width 0) layout-center-width]
        [else 0]))

    (define (unaligned-or-center-elements)
      (if (not (empty? layout-center-elements))
          layout-center-elements
          layout-unaligned-elements))
    
    (define (layout-snip e total-width y ew eh)
      (if (< (- total-width (+ layout-left-width layout-right-width (unaligned-or-center-width))) ew)
          ; we don't have room for this element on the current line/y-position
          (let ([new-y (next-line-y-pos y)])
            (if (not (= y new-y))
                (begin
                  (layout-goto-new-line new-y)
                  (layout-snip e total-width new-y ew eh))
                ; we have advanced the current y position as far as we can and it still doesn't fit
                (begin
                  (case (element-alignment e)
                    [(left)
                     (set! layout-left-elements (cons e layout-left-elements))
                     (set! layout-left-width (+ ew snip-xmargin))]
                    [(right)
                     (set! layout-right-elements (cons e layout-right-elements))
                     (set! layout-right-width (+ ew snip-xmargin))]
                    [else
                     (set! layout-unaligned-elements (cons e layout-unaligned-elements))
                     (set! layout-unaligned-width (+ ew snip-xmargin))])
                  (values 0 y ew (+ y eh)))))
          ; we do have room
          (case (element-alignment e)
            [(left)
             ;(printf "layout left aligned element~n")
             (if (empty? layout-left-elements)
                 (begin
                   (set! layout-left-elements (cons e layout-left-elements))
                   (set! layout-left-width (+ ew snip-xmargin))
                   ; shift unaligned elements over since we inserted this element to the left
                   (adjust-elements-xpos! layout-unaligned-elements (+ ew snip-xmargin))
                   (values 0 y ew (+ y eh)))
                 (let ([x1 layout-left-width]
                       [y1 y]
                       [x2 (+ layout-left-width ew)]
                       [y2 (+ y eh)])
                   (set! layout-left-elements (cons e layout-left-elements))
                   (set! layout-left-width (+ layout-left-width ew snip-xmargin))
                   ; shift unaligned elements over since we inserted this element to the left
                   (adjust-elements-xpos! layout-unaligned-elements (+ ew snip-xmargin))
                   (values x1 y1 x2 y2)))]
            [(right)
             ;(printf "layout right aligned element~n")
             (if (empty? layout-right-elements)
                 (begin
                   (set! layout-right-elements (cons e layout-right-elements))
                   (set! layout-right-width (+ ew snip-xmargin))
                   (values (- total-width ew) y total-width (+ y eh)))
                 (let ([x1 (- total-width layout-right-width ew)]
                       [y1 y]
                       [x2 (- total-width layout-right-width)]
                       [y2 (+ y eh)])
                   (set! layout-right-elements (cons e layout-right-elements))
                   (set! layout-right-width (+ layout-right-width ew snip-xmargin))
                   (values x1 y1 x2 y2)))]
            [(center)
             (printf "layout center aligned element~n")
             ; we assume that center and unaligned elements are mutually exclusive on the same line
             ; the canvas user must end a line before adding an element with the other alignment
             ; otherwise, behavior is undefined
             (if (empty? layout-center-elements)
                 (let ([margin (/ (- total-width layout-left-width layout-right-width ew) 2)])
                   (set! layout-center-elements (cons e layout-center-elements))
                   (set! layout-center-width (+ ew snip-xmargin))
                   (values (+ layout-left-width margin) y (+ layout-left-width margin ew) (+ y eh)))
                 (let* ([margin (/ (- total-width layout-left-width layout-right-width layout-center-width ew) 2)]
                        [pos (+ layout-left-width margin)])
                   ; reposition each centered element on the line
                   (for ([e (in-list layout-center-elements)])
                     (set-element-xpos! e pos)
                     (set! pos (+ pos (get-element-width e) snip-xmargin)))
                   (set! layout-center-elements (cons e layout-center-elements))
                   (set! layout-center-width (+ layout-center-width ew snip-xmargin))
                   (values pos y (+ pos ew) (+ y eh))))]
            [(unaligned)
             ;(printf "layout unaligned element~n")
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
                     ; adjust y position to touch the baseline
                     (define diff (- layout-baseline-pos y2))
                     (set! y1 (+ y1 diff))
                     (set! y2 (+ y2 diff)))
                   (set! layout-unaligned-elements (cons e layout-unaligned-elements))
                   (set! layout-unaligned-width (+ layout-unaligned-width ew snip-xmargin))
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
      (set! layout-center-elements '())
      (set! layout-left-width 0)
      (set! layout-right-width 0)
      (set! layout-unaligned-width 0)
      (set! layout-center-width 0)
      (set! layout-baseline-pos 0)
      
      (for ([e (in-dlist elements)])
        (place-element e place-x place-y))
      
      (when (> (dlist-length elements) 0)
        (set-visible-elements!)))
    
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
             ;(printf "place-element ~a~n" (element-snip e))
             ;; calculate the extent of individual words
             ;; only have to do this once
             (when (not (element-words e))
               (calc-word-extents e))
             ;; calculate the extent of the text with word wrapping
             (define font (send (get-style e) get-font))
             (define-values (width height descent space) (send dc get-text-extent "a" font)) ; only need height, so string doesn't matter
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
               (define-values (last-line-width unused-h unused-d unused-s) (send dc get-text-extent (substring (element-snip e) start-pos) font))
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
             ;; calculate the extent of individual words
             ;; only have to do this once
             (when (not (element-words e))
               (calc-word-extents e))
             (set!-values (x1 y1 x2 y2) (layout-string e dw y))
             (printf "layout placed ~a (~a,~a)-(~a,~a) left:~a, una:~a, right:~a~n" (element-alignment e) x1 y1 x2 y2 layout-left-width layout-unaligned-width layout-right-width)
             ;; set position for adding next element
             (when (element-end-of-line e)
                 (layout-goto-new-line (add1 y2)))]
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
               (set!-values (x1 y1 x2 y2) (layout-snip e dw y (unbox snip-w) (unbox snip-h)))
               (printf "layout placed ~a (~a,~a)-(~a,~a) left:~a, una:~a, right:~a~n" (element-alignment e) x1 y1 x2 y2 layout-left-width layout-unaligned-width layout-right-width)
               ; layout-goto-new-line needs the element's position to be set, so set it early for now
               (set-element-xpos! e x1)
               (set-element-ypos! e y1)
               ; set position for adding next element
               ; should we allow left or right aligned elements be marked as end-of-line?
               (when (element-end-of-line e)
                 (layout-goto-new-line (add1 y2)))]
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
      (define-values (left top) (get-view-start))

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
            ;(printf "  snip at ~ax~a, text=~a~n" x y  (element-snip e))
            (if (and (wrap-text?) (string? (element-snip e)))
                (draw-wrapped-text e dc left top)
                (draw e dc
                      x y
                      x y
                      (- cw xmargin) (- ch ymargin)
                      0 0)))
          ;; clear top, bottom and right margins in case they were drawn to
          ;; top is needed for cases where an element is partially above the viewable area
          (clear-rectangle 0 0 cw ymargin)
          (clear-rectangle 0 (- ch ymargin) cw ymargin)
          (clear-rectangle (- cw xmargin) 0 xmargin ch))
        (lambda ()
          (send dc resume-flush))))

    (define/override (on-scroll event)
      (define-values (dw dh) (get-drawable-size))

      ;(printf "on-scroll: ~a ~a~n" (send event get-direction) (send event get-position))
      
      (if (eq? (send event get-direction) 'vertical)
          (let* ([top (send event get-position)]
                 [bottom (+ top dh)]
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

      (when (not visible-elements)
        (set-visible-elements!))
      
      ;; reposition all elements
      (when (and (wrap-text?) (not (= cached-client-width cw)))
        (printf "on-size canvas ~ax~a " canvas-width canvas-height)
        (reset-layout)
        (printf "-> ~ax~a~n" canvas-width canvas-height))

      ;; update visible elements if window height changes
      (when visible-elements
        (if (> ch cached-client-height)
            (adjust-visible-elements-forward! visible-elements top bottom)
            (adjust-visible-elements-back! visible-elements top bottom)))
      ;(printf "on-size: # visible = ~a~n" (dlist-length visible-elements))
      (set! cached-client-width cw)
      (set! cached-client-height ch)

      ;; need to update the scroll range when the client size changes
      (define-values (vx vy) (get-virtual-size))
      (update-scrollbars vx vy))

    (define (select-element x y)
      (for/or ([e (in-dlist visible-elements)]
               #:when (and (>= y (element-ypos e))
                           (>= x (element-xpos e))))                             
        (define w (box 0))
        (define h (box 0))
        (get-extent e dc (element-xpos e) (element-ypos e) w h)
        (and (<= y (+ (element-ypos e) (unbox h)))
             (<= x (+ (element-xpos e) (unbox w)))
             e)))

    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (case (send event get-key-code)
        [(wheel-up wheel-down)
         (define max-scroll (get-scroll-range 'vertical))
         (define scroll-pos (get-scroll-pos 'vertical))
         (define new-scroll-pos
           (if (eq? key-code 'wheel-up)
               (max 0 (- scroll-pos wheel-step))
               (min max-scroll (+ scroll-pos wheel-step))))
         (printf "new scroll-y ~a, max ~a~n" scroll-y max-scroll)
         (scroll-to new-scroll-pos)]
        [(next)
         (define-values (dw dh) (get-drawable-size))
         (define-values (x y) (get-view-start))
         (printf "page down to ~a~n" (+ y dh))
         (scroll-to (+ y dh))]
        [(prior)
         (define-values (dw dh) (get-drawable-size))
         (define-values (x y) (get-view-start))
         (printf "page up to ~a~n" (- y dh))
         (scroll-to (- y dh))]
        [(home)
         (scroll-to 0)]
        [(end)
         (define-values (vx vy) (get-virtual-size))
         (define-values (dw dh) (get-drawable-size))
         (scroll-to (- vy dh))]))

    ;; store the element that has mouse focus. send on-goodbye-event to an element's snip if it loses focus
    (define element-with-focus #f)
    
    (define/override (on-event event)
      (case (send event get-event-type)
        [(left-down middle-down right-down motion)
         (define-values (left top) (get-view-start))
         (define-values (ex ey) (values (send event get-x)
                                        (send event get-y)))
         (define-values (x y) (values (- (+ ex left) xmargin)
                                      (- (+ ey top) ymargin)))
         (define e (select-element x y))

         ;; send on-goodby-event to snips that lose mouse focus
         (when (not (eq? e element-with-focus))
           (when (and element-with-focus (is-a? (element-snip element-with-focus) snip%))
             (send (element-snip element-with-focus) on-goodbye-event dc x y (element-xpos element-with-focus) (element-ypos element-with-focus) event))
           (set! element-with-focus e))
                
         (when (and e (is-a? (element-snip e) snip%))
           (printf "on-event: pass event to snip ~ax~a, canvas:~ax~a, element at ~ax~a~n"
                   (send event get-x) (send event get-y) x y (element-xpos e) (element-ypos e))
           (send (element-snip e) on-event
                 dc
                 (+ (- (element-xpos e) left) xmargin)
                 (+ (- (element-ypos e) top) ymargin)
                 (+ (- (element-xpos e) left) xmargin)
                 (+ (- (element-ypos e) top) ymargin)
                 event))]
        [else
         void]))

    (define/public (get-mode)
      mode)

    ;;
    (define/public (set-mode m)
      (set! mode m)
      (unless (dlist-empty? elements)
        (reset-layout)
        (let-values ([(x y) (get-virtual-size)])
          (update-scrollbars x y))
        (refresh)))

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

    (define/public (scroll-to y)
      (define-values (dw dh) (get-drawable-size))
      (define max-scroll (get-scroll-range 'vertical))
      (define old-scroll-pos scroll-y)
      (define new-scroll-pos
        (if (> y max-scroll)
            max-scroll
            (if (< y 0)
                0
                y)))
      
      (when (not (= new-scroll-pos old-scroll-pos))
        (set! scroll-y new-scroll-pos)
        (set-scroll-pos 'vertical new-scroll-pos)
        
        (if (not visible-elements)
            (set-visible-elements!)
            (update-visible-elements! (- new-scroll-pos old-scroll-pos) scroll-y (+ scroll-y dh)))
        (refresh)))

    ;; layout-canvas% users don't have direct access to the elements, so they may need to
    ;; find an element's position using the snip(or string) that they added to the canvas
    (define/public (lookup-snip-position-size s)
      (define elem
        (for/first ([e (in-dlist elements)]
                    #:when (eq? (element-snip e) s))
          e))

      (cond
        [(and elem (or (false? (element-lines elem))
                       (empty? (element-lines elem))))
         (values (element-xpos elem)
                 (element-ypos elem)
                 (get-element-width elem)
                 (get-element-height elem))]
        [elem
         (values (element-xpos elem)
                 (element-ypos elem)
                 (get-element-width (car (element-lines elem)))
                 (get-element-height (car (element-lines elem))))]
        [else
         (error "lookup-snip-position-size failed to find snip!")]))

    (define/public (first-visible-snip)
      (if (and visible-elements (dlist-head-value visible-elements))
          (element-snip (dlist-head-value visible-elements))
          #f))
          
    ;; add element e to the end of the elements dlist and update visible elements
    (define (append-element e)
      (define-values (dw dh) (get-drawable-size))
      (dlist-append! elements e)
      ; send a positive value to for change argument to trigger a check to expand tail of the visible-elements list
      (update-visible-elements! 1 scroll-y (+ scroll-y dh)))
    
    ;; append a snip. snips have their own style
    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned])
      (define e (element s end-of-line alignment))
      (place-element e place-x place-y)
      (define-values (vx vy) (get-virtual-size))
      (update-scrollbars vx vy)
      (append-element e))
    
    ;; append a string using the default style and alignment (or provided style and alignment)
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
           (define-values (vx vy) (get-virtual-size))
           (update-scrollbars vx vy)
           (append-element e))]
        [else
         (define e (element s end-of-line alignment))
         (set-element-text-style! e (or style default-style))
         (place-element e place-x place-y)
         (define-values (vx vy) (get-virtual-size))
         (update-scrollbars vx vy)
         (append-element e)]))
      
    (define/public (get-style-list) styles)

    (define/public (set-default-stlye style-name)
      (define style (send styles find-named-style style-name))
      (if style
          (set! default-style style)
          #f))

    (define in-edit-sequence #f)
    
    (define/public (begin-edit-sequence)
      (set! in-edit-sequence #t))

    (define/public (in-edit-sequence?)
      in-edit-sequence)
    
    (define/public (end-edit-sequence)
      (set! in-edit-sequence #f))))


(module+ main  
  (define frame 
    (new (class frame% (super-new))
         [label "Layout canvas test"]
         [width 800]
         [height 600]))

  (define canvas
    (new layout-canvas% (parent frame)
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
      (send canvas set-mode 'wrapped))

  (send frame show #t)

  (define highlander-text "He is immortal. Born in the highlands of Scotland 400 years ago, there are others like him. Some good, some evil. For centuries he has battled the forces of darkness with holy ground his only refuge. He cannot die unless you take his head and with it his power. There can be only one. He is Duncan Macleod, the Highlander!\n")
  (define test-selector "gamefaqs-archive/ps2/final-fantasy-xii/FAQ_Walkthrough-by--Berserker.txt")
  ;(define test-selector "/media/floppies.txt")
  ;(define test-selector ".")

  (if layout-test
      (let ([square (make-object image-snip% "test/square.png")]
            [square-left (make-object image-snip% "test/square-left.png")]
            [square-right (make-object image-snip% "test/square-right.png")]
            [square-center (make-object image-snip% "test/square-center.png")]
            [big (make-object image-snip% "test/big.png")]
            [tall (make-object image-snip% "test/tall.png")]
            [tall-left (make-object image-snip% "test/tall-left.png")]
            [tall-right (make-object image-snip% "test/tall-right.png")])

        (case layout-test
          [(text1)
           (send canvas append-snip square)
           (send canvas append-snip square-left #f 'left)
           (send canvas append-string "Testing, testing. " #f #f)
           (send canvas append-string highlander-text #f #f)
           (send canvas append-snip square)]
          
          [(text2)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-string "Here is some left aligned text. Here is some left aligned text. Here is some left aligned text." #f #t 'left)
           (send canvas append-string "\n")
           (send canvas append-string "Here is some unaligned text. Here is some unaligned text. Here is some unaligned text." #f #t)
           (send canvas append-string "\n")
           (send canvas append-string "Here is some right aligned text. Here is some right aligned text. Here is some right aligned text." #f #t 'right)
           (send canvas append-string "\n")
           (send canvas append-string "Here is some centered text. Here is some centered text. Here is some centered text." #f #t 'center)
           (send canvas append-string "\n")
           (send canvas append-snip square-left #f 'left)
           (send canvas append-snip square-right #f 'right)
           (send canvas append-snip square)
           (send canvas append-snip square)
           (send canvas append-snip tall #t)]
          [(center)
           (send canvas append-string "Layout Test" (send (send canvas get-style-list) find-named-style "Header1") #t 'center)
           (send canvas append-snip square-center #t 'center)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-snip square #t)
           (send canvas append-snip square-center #f 'center)
           (send canvas append-snip square-center #f 'center)
           (send canvas append-snip square-center #t 'center)
           
           (send canvas append-snip square-right #f 'right)
           (send canvas append-snip tall-left #f 'left)
           (send canvas append-snip square-center #t 'center)
           (send canvas append-snip square-center #t 'center)
           (send canvas append-snip square)
           (send canvas append-snip square)]
          [(large)
           ;(send canvas append-snip square)
           ;(send canvas append-snip tall)
           ;(send canvas append-snip square #t)
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
  
  (printf "append finished~n"))
