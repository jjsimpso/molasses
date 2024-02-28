#lang racket/gui

(require racket/class
         racket/snip
         racket/draw
         data/gvector)

(require "dlist.rkt")

(provide table-snip%)

(define cell%
  (class object% (super-new)
    (init drawing-context
          defstyle
          [colspan 1]
          [valign 'middle]
          [bgcolor #f]
          [width #f]
          [horiz-margin 5]
          [vert-margin 5])

    (define dc drawing-context)
    (define default-style defstyle)
    (define vert-align valign)
    (define column-span colspan)
    (define background-color bgcolor)
    (define fixed-width
      (if (and width (> width 1))
          width
          0))
    (define relative-width
      (if (and width (< width 1) (> width 0))
          width
          0))
    ;; 
    (define xmargin horiz-margin)
    (define ymargin vert-margin)
    ;; upper left corner of cell in table's virtual canvas
    (define canvas-x 0)
    (define canvas-y 0)
    ;;
    (define cell-width (* 2 horiz-margin))
    (define cell-height (* 2 vert-margin))
    ;; size of cell's contents not including borders and margins
    (define content-width 0)
    (define content-height 0)

    (define valign-offset 0)
    
    (define snip-xmargin 5)
    (define snip-ymargin 2)

    ;; current position to place next element
    (define place-x 0)
    (define place-y 0)

    (define (set-valign-offset)
      (define ypadding (* 2 ymargin))
      (define height (- cell-height ypadding))
      (set! valign-offset
        (case vert-align
          [(top) 0]
          [(bottom)
           (- height content-height)]
          [else ;middle
           (quotient (- height content-height) 2)]))
      #;(printf "setting valign-offset to ~a(~a/~a)~n" valign-offset height content-height))

    (define/public (get-dc)
      dc)

    (define/public (get-colspan)
      column-span)

    (define/public (get-fixed-width)
      (if (> fixed-width 0)
          (+ fixed-width (* 2 xmargin))
          0))

    (define/public (get-relative-width)
      relative-width)

    (define/public (calc-relative-width table-width)
      (if (= relative-width 0)
          0
          (let ([w (* table-width relative-width)]
                [margin (* 2 xmargin)])
            (if (< margin w)
                (- w margin)
                margin))))

    (define/public (get-position)
      (values canvas-x canvas-y))

    (define/public (set-position posx posy)
      ;(printf " set cell position to ~a,~a~n" posx posy)
      (set! canvas-x posx)
      (set! canvas-y posy))

    (define/public (get-size)
      (values cell-width cell-height))
    
    (define/public (set-size w h)
      (set! cell-width w)
      (set! cell-height h)
      (set-valign-offset))

    (define/public (get-width)
      cell-width)

    (define/public (get-height)
      cell-height)
    
    (define/public (set-width w)
      (set! cell-width w))

    (define/public (set-height h)
      (set! cell-height h)
      (set-valign-offset))
    
    (define/public (get-content-size)
      (values (+ content-width (* 2 xmargin))
              (+ content-height (* 2 ymargin))))

    (define/public (get-drawable-size)
      (define-values (w h) (get-size))
      (values (- w (* 2 xmargin))
              (- h (* 2 ymargin))))

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
       [properties #:mutable]
       [xpos #:mutable #:auto] ; position of top left corner of element, #f for hidden(hiding not implemented yet)
       [ypos #:mutable #:auto]
       [text-style #:mutable #:auto] ; only used for strings since snips have their own style
       [cached-text-extent #:mutable #:auto]
       [words #:mutable #:auto]
       [lines #:mutable #:auto]) ; list of wrapped-line's
      #:prefab #:auto-value #f)

    ;; list of all elements in order of insertion
    (define elements (dlist-new))

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

    (define (get-style e)
      (or (element-text-style e)
          (send (element-snip e) get-style)))

    (define (draw-wrapped-text e dc ox oy)
      (for ([line (in-list (element-lines e))])
        ;(printf "draw-wrapped-text: (~a,~a) ~a~n" (+ oy (wrapped-line-x line)) (+ oy (wrapped-line-y line)) (substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line)))
        (send/apply dc draw-text `(,(substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line)) ,(+ (wrapped-line-x line) ox) ,(+ (wrapped-line-y line) oy)))))
    
    (define (draw-element e dc x y left top right bottom dx dy)
      ;(printf "draw-element at ~a,~a~n" x y)
      (if (string? (element-snip e))
          (send dc draw-text (element-snip e) x y)
          (send (element-snip e) draw dc x y left top right bottom dx dy 'no-caret)))

    (define (draw-background dc x y)
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      (define old-brush (send dc get-brush))
      
      (send dc set-smoothing 'aligned)
      (send dc set-pen (make-color 0 0 0) 0 'transparent)
      (send dc set-brush background-color 'solid)
      (send dc draw-rectangle x y cell-width cell-height)
      
      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen)
      (send dc set-brush old-brush))
    
    ;; x and y are the upper left corner of the cell in canvas dc coordinates
    (define/public (draw dc x y left top right bottom dx dy)
      (define current-style #f)
      ;(printf "drawing cell at ~a,~a~n" x y)

      (when background-color
        (draw-background dc x y))
      
      (for ([e (in-dlist elements)])
        ;; set the style if it has changed
        (when (not (eq? (get-style e) current-style))
          (set! current-style (get-style e))
          (send current-style switch-to dc #f))
        (define-values (xpos ypos) (values (+ x (element-xpos e) xmargin)
                                           (+ y (element-ypos e) ymargin valign-offset)))
        (if (string? (element-snip e))
            (draw-wrapped-text e dc (+ x xmargin) (+ y ymargin valign-offset))
            (draw-element e dc
                      xpos ypos
                      xpos ypos
                      (+ x (- cell-width xmargin)) (+ y (- cell-height ymargin))
                      0 0))))

    (define (clip-lines lines x y)
      (for/or ([wl (in-dlist lines)])
        (and (>= x (wrapped-line-x wl))
             (>= y (wrapped-line-y wl))
             (<= x (+ (wrapped-line-x wl) (wrapped-line-w wl)))
             (<= y (+ (wrapped-line-y wl) (wrapped-line-h wl))))))
    
    (define (select-element x y)
      (for/or ([e (in-dlist elements)]
               #:when (and (>= y (element-ypos e))
                           (>= x (element-xpos e))))                             
        (define w (box 0))
        (define h (box 0))
        (get-extent e dc (element-xpos e) (element-ypos e) w h)
        (and (<= y (+ (element-ypos e) (unbox h)))
             (<= x (+ (element-xpos e) (unbox w)))
             (or (false? (element-lines e))
                 (clip-lines (element-lines e) x y))
             e)))

    ;; store the element that has mouse focus. send on-goodbye-event to an element's snip if it loses focus
    (define element-with-focus #f)

    (define (check-element-enter-leave e x y event)
      (when (not (eq? e element-with-focus))
        ;; send on-goodbye-event to snips that lose mouse focus
        (when (and element-with-focus (is-a? (element-snip element-with-focus) snip%))
          (send (element-snip element-with-focus) on-goodbye-event dc x y (element-xpos element-with-focus) (element-ypos element-with-focus) event))
        ;; send enter event when a new snip gets focus
        (when (and e (is-a? (element-snip e) snip%))
          (send (element-snip e) adjust-cursor dc x y (- x (element-xpos e)) (- y (element-ypos e)) (new mouse-event% [event-type 'enter])))
        (set! element-with-focus e)))

    ;; if mouse moves quickly, the cell could lose focus without receiving any mouse events,
    ;; so we need a function to clean things up. called by cell's table.
    (define/public (leave-cell dc x y event)
      (printf "leave-cell~n")
      (when element-with-focus
        (when (and element-with-focus (is-a? (element-snip element-with-focus) snip%))
          (send (element-snip element-with-focus) on-goodbye-event dc x y (element-xpos element-with-focus) (element-ypos element-with-focus) event))
        (set! element-with-focus #f)))
    
    (define/public (on-event dc x y event)
      ; event coordinates in cell coords, ignoring cell padding
      (define-values (cx cy) (values (- x canvas-x xmargin)
                                     (- y canvas-y ymargin valign-offset)))
      (define e (select-element cx cy))

      ;(printf "cell on-event ~ax~a, canvas ~ax~a, cell coord ~ax~a~n" x y canvas-x canvas-y cx cy)

      (case (send event get-event-type)
        [(left-down middle-down right-down motion)
         (check-element-enter-leave e cx cy event)
         
         (when (and e (is-a? (element-snip e) snip%))
           #;(printf "on-event: pass event at ~ax~a to snip coords ~ax~a, canvas:~ax~a, element at ~ax~a~n"
                 (send event get-x) (send event get-y)
                 (+ (element-xpos e) xmargin) (+ (element-ypos e) ymargin valign-offset)
                 x y
                 (element-xpos e) (element-ypos e))
           (send (element-snip e) on-event
                 dc
                 (+ (element-xpos e) xmargin)
                 (+ (element-ypos e) ymargin valign-offset)
                 (+ (element-xpos e) xmargin)
                 (+ (element-ypos e) ymargin valign-offset)
                 event))]
        [else
         void]))
    
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
      ;(printf "adjust-elements-xpos!: delta=~a~n" xdelta)
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

    (define (unaligned-or-center-width)
      (cond
        [(> layout-unaligned-width 0) layout-unaligned-width]
        [(> layout-center-width 0) layout-center-width]
        [else 0]))

    (define (unaligned-or-center-elements)
      (if (not (empty? layout-center-elements))
          layout-center-elements
          layout-unaligned-elements))
    
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
                  (pop-layout-list (cdr ll) (- lwidth w (if (wrapped-line? e) 0 snip-xmargin)) new-y)
                  (values ll lwidth))))))

    (define (bottom-edge-of-elements ll)
      (if (not (empty? ll))
          (+ (get-element-y (car ll))
             (get-element-height (car ll)))
          (error "empty list")))
    
    ;; center and unaligned elements should be considered to all be on one line
    ;; the bottom edge is the bottom edge of the entire line and not just the
    ;; last element.
    (define (bottom-edge-of-line ll)
      (apply max (map (lambda (e) (+ (get-element-y e)
                                     (get-element-height e)))
                      ll)))

    (define (next-line-y-pos original)
      (+ snip-ymargin
       (cond
         [(not (empty? layout-unaligned-elements))
          ;(printf "beol unaligned ~a~n" (bottom-edge-of-line layout-unaligned-elements))
          (bottom-edge-of-line layout-unaligned-elements)]
         [(not (empty? layout-center-elements))
          (bottom-edge-of-line layout-center-elements)]
         [(not (empty? layout-left-elements))
          (if (empty? layout-right-elements)
              (bottom-edge-of-elements layout-left-elements)
              (min (bottom-edge-of-elements layout-left-elements)
                   (bottom-edge-of-elements layout-right-elements)))]
         [(not (empty? layout-right-elements))
          (bottom-edge-of-elements layout-right-elements)]
         [else
          ; return original
          #;(printf "next-line-y-pos: all layout lists are empty~n")
          (- original snip-ymargin)])))

    (define (layout-goto-new-line new-y)
      ;(printf "layout-goto-new-line: ~a~n" new-y)
      (set! place-x 0) ; place-x value isn't currently used in layout mode
      (set! place-y new-y)
      (set! layout-baseline-pos new-y)
      (set!-values (layout-left-elements layout-left-width) (pop-layout-list layout-left-elements layout-left-width new-y))
      (set!-values (layout-right-elements layout-right-width) (pop-layout-list layout-right-elements layout-right-width new-y))
      (set!-values (layout-center-elements layout-center-width) (pop-layout-list layout-center-elements layout-center-width new-y))
      ;(printf "before pop: ~a ~a~n" layout-unaligned-elements layout-unaligned-width)
      (set!-values (layout-unaligned-elements layout-unaligned-width) (pop-layout-list layout-unaligned-elements layout-unaligned-width new-y)))

    (define (layout-string e total-width y)
      (define (layout-remainder-of-line word-list space-available)
        ;(printf "layout-remainder-of-line: space-available=~a~n" space-available)
        (let loop ([words word-list]
                   [last-word #f]
                   [width 0])
          (if (empty? words)
              (values last-word width '())
              (let ([w (car words)])
                ;(printf "layout-remainder-of-line: loop iter width=~a, ~a, space-avail=~a~n" width (word-to-next w) space-available)
                (cond
                  [(< (+ width (word-to-next w)) space-available)
                   (loop (cdr words)
                         w
                         (+ width (word-to-next w)))]
                  [else
                   ;; end of line but check if w fits on the current line
                   (define maybe-width (+ width (word-width w)))
                   (if (<= maybe-width space-available)
                       (values w maybe-width (cdr words))
                       ; w doesn't fit on line. last-word could be #f here if this was our first iteration
                       (values last-word width words))])))))

      (define font (send (get-style e) get-font))
      (define-values (font-width font-height font-descent font-space) (send dc get-text-extent "a" font)) ; only need height, so string doesn't matter
      (define height (- font-height font-descent))
      (define to-next-y (+ font-height 1))
      ;; unaligned and center elements must consider the lowest point on the line
      ;; some text on the line could be in a different font and have a lower descent
      ;; from the baseline
      (define (calc-next-line-y y ll)
        (if (empty? ll)
            (+ y to-next-y)
            (max (+ y to-next-y)
                 (+ (bottom-edge-of-line ll) 1))))

      ;(printf "layout-string: tw=~a, y=~a~n" total-width y)
      
      (case (element-alignment e)
        [(right)
         (let loop ([words (element-words e)]
                    [lines '()]
                    [line-start-pos 0]
                    [ypos y]
                    [x (- total-width layout-right-width)]
                    [max-width 0])
           ;(printf "loop right: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
           (define-values (last-word width remaining-words)
             (layout-remainder-of-line words (- total-width layout-left-width (unaligned-or-center-width) layout-right-width)))
           ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
           (cond
             [(and (false? last-word) (empty? remaining-words))
              ; final iteration
              ;(printf " final iteration~n")
              ;; add last line to layout element list
              (set! layout-right-elements (cons (car lines) layout-right-elements))
              (set! layout-right-width (+ layout-right-width (wrapped-line-w (car lines))))
              ;(printf " end line width=~a, lrw=~a~n" (wrapped-line-w (car lines)) layout-right-width)
              ;; set the element's lines field and put the lines in order
              (set-element-lines! e (reverse lines))
              (values x y (+ x max-width) (+ ypos font-height))]
             [(and (false? last-word) (empty? (unaligned-or-center-elements)) (empty? layout-left-elements) (empty? layout-right-elements))
              ; first word is too long to fit on a line, just place it at the beginning of line
              ;(printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
              (define xpos 0)
              (define new-ypos ypos)
              (when (not (empty? (cdr remaining-words)))
                (set! new-ypos (+ ypos to-next-y))
                (layout-goto-new-line new-ypos))
              (loop (cdr remaining-words)
                    (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                    (word-end-pos (car remaining-words))
                    new-ypos
                    (min xpos x)
                    (max width max-width))]
             [(false? last-word)
              ; first word is too long, advance line and try again
              (define new-ypos (+ ypos to-next-y))
              (layout-goto-new-line new-ypos)
              (loop remaining-words
                    lines
                    line-start-pos
                    new-ypos
                    x
                    max-width)]
             [else
              (define xpos (- total-width layout-right-width width))
              (define new-ypos ypos)
              (when (not (empty? remaining-words))
                (set! new-ypos (+ ypos to-next-y))
                (layout-goto-new-line new-ypos))
              (loop remaining-words
                    (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                    (word-end-pos last-word)
                    new-ypos
                    (min xpos x)
                    (max width max-width))]))]
        [(center)
         (define baseline (+ y height))
         (when (> baseline layout-baseline-pos)
             (when (not (empty? layout-center-elements))
               (define diff (- baseline layout-baseline-pos))
               (adjust-elements-ypos! layout-center-elements diff))
             (set! layout-baseline-pos baseline))
         
         (let loop ([words (element-words e)]
                    [lines '()]
                    [line-start-pos 0]
                    [ypos (- layout-baseline-pos height)]
                    [x total-width]
                    [max-width 0])
           ;(printf "loop center: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
           (define-values (last-word width remaining-words)
             (layout-remainder-of-line words (- total-width layout-left-width layout-center-width layout-right-width)))
           ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
           (cond
             [(and (false? last-word) (empty? remaining-words))
              ; final iteration
              ;(printf " final iteration~n")
              ;; update the baseline position after all lines are placed
              (set! layout-baseline-pos (+ ypos height))
              ;; add last line to layout element list
              (set! layout-center-elements (cons (car lines) layout-center-elements))
              (set! layout-center-width (+ layout-center-width (wrapped-line-w (car lines))))
              ;(printf " end line width=~a, lcw=~a~n" (wrapped-line-w (car lines)) layout-center-width)
              ;; set the element's lines field and put the lines in order
              (set-element-lines! e (reverse lines))
              (values x y (+ x max-width) (+ ypos font-height))]
             [(and (not (empty? layout-center-elements)) (false? last-word))
              ; there is a partial line of centered elements
              ; first word is too long, advance line and try again
              (define new-ypos (calc-next-line-y ypos layout-center-elements))
              (layout-goto-new-line new-ypos)
              (loop remaining-words
                    lines
                    line-start-pos
                    new-ypos
                    x
                    max-width)]
             [(not (empty? layout-center-elements))
              ; there is a partial line of centered elements, shift existing elements over to make room
              (define old-margin ( / (- total-width layout-left-width layout-center-width layout-right-width) 2))
              (define margin ( / (- total-width layout-left-width layout-center-width layout-right-width width) 2))
              (define diff (- margin old-margin))
              ;(printf "shift centered elements over: lcw=~a, width =~a, margin ~a to ~a, diff=~a~n" layout-center-width width old-margin margin diff)
              (adjust-elements-xpos! layout-center-elements diff)
              ;;
              (define xpos (+ layout-left-width margin layout-center-width))
              (define new-ypos ypos)
              (when (not (empty? remaining-words))
                (set! new-ypos (calc-next-line-y ypos layout-center-elements))
                (layout-goto-new-line new-ypos))
              (loop remaining-words
                    (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                    (word-end-pos last-word)
                    new-ypos
                    (min xpos x)
                    (max width max-width))]
              [(and (false? last-word) (empty? layout-center-elements) (empty? layout-left-elements) (empty? layout-right-elements))
              ; first word is too long to fit on a line, just place it at the beginning of line
              ;(printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
              (define xpos 0)
              (define new-ypos ypos)
              (when (not (empty? (cdr remaining-words)))
                (set! new-ypos (calc-next-line-y ypos layout-center-elements))
                (layout-goto-new-line new-ypos))
              (loop (cdr remaining-words)
                    (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                    (word-end-pos (car remaining-words))
                    new-ypos
                    (min xpos x)
                    (max width max-width))]
             [(false? last-word)
              ; first word is too long, advance line and try again
              (define new-ypos (calc-next-line-y ypos layout-center-elements))
              (layout-goto-new-line new-ypos)
              (loop remaining-words
                    lines
                    line-start-pos
                    new-ypos
                    x
                    max-width)]
             [else
              (define space-available (- total-width layout-left-width layout-right-width))
              (define space-leftover (- space-available width))
              (define xpos (+ layout-left-width (/ space-leftover 2)))
              (define new-ypos ypos)
              (when (not (empty? remaining-words))
                (set! new-ypos (calc-next-line-y ypos layout-center-elements))
                (layout-goto-new-line new-ypos))
              (loop remaining-words
                    (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                    (word-end-pos last-word)
                    new-ypos
                    (min xpos x)
                    (max width max-width))]))]
        [(left)
         (let loop ([words (element-words e)]
                    [lines '()]
                    [line-start-pos 0]
                    [ypos y]
                    [x layout-left-width]
                    [max-width 0])
           ;(printf "loop left: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
           (define-values (last-word width remaining-words)
             (layout-remainder-of-line words (- total-width layout-left-width (unaligned-or-center-width) layout-right-width)))
           ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
           (cond
             [(and (false? last-word) (empty? remaining-words))
              ; final iteration
              ;(printf " final iteration~n")
              ;; add last line to layout element list
              (set! layout-left-elements (cons (car lines) layout-left-elements))
              (set! layout-left-width (+ layout-left-width (wrapped-line-w (car lines))))
              ;(printf " end line width=~a, llw=~a~n" (wrapped-line-w (car lines)) layout-left-width)
              ;; set the element's lines field and put the lines in order
              (set-element-lines! e (reverse lines))
              (values x y (+ x max-width) (+ ypos font-height))]
             [(and (false? last-word) (empty? (unaligned-or-center-elements)) (empty? layout-left-elements) (empty? layout-right-elements))
              ; first word is too long to fit on a line, just place it at the beginning of line
              ;(printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
              (define xpos 0)
              (define new-ypos ypos)
              (when (not (empty? (cdr remaining-words)))
                (set! new-ypos (+ ypos to-next-y))
                (layout-goto-new-line new-ypos))
              (loop (cdr remaining-words)
                    (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                    (word-end-pos (car remaining-words))
                    new-ypos
                    (min xpos x)
                    (max width max-width))]
             [(false? last-word)
              ; first word is too long, advance line and try again
              (define new-ypos (+ ypos to-next-y))
              (layout-goto-new-line new-ypos)
              (loop remaining-words
                    lines
                    line-start-pos
                    new-ypos
                    x
                    max-width)]
             [else
              (define xpos layout-left-width)
              (define new-ypos ypos)
              (when (not (empty? remaining-words))
                (set! new-ypos (+ ypos to-next-y))
                (layout-goto-new-line new-ypos))
              (loop remaining-words
                    (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                    (word-end-pos last-word)
                    new-ypos
                    (min xpos x)
                    (max width max-width))]))]
        [(unaligned)
         (define baseline (+ y height))
         (when (> baseline layout-baseline-pos)
             (when (not (empty? layout-unaligned-elements))
               (define diff (- baseline layout-baseline-pos))
               (adjust-elements-ypos! layout-unaligned-elements diff))
             (set! layout-baseline-pos baseline))

         (let loop ([words (element-words e)]
                    [lines '()]
                    [line-start-pos 0]
                    [ypos (- layout-baseline-pos height)]
                    [x (+ layout-left-width layout-unaligned-width)]
                    [max-width 0])
           ;(printf "loop unaligned: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
           (define-values (last-word width remaining-words)
             (layout-remainder-of-line words (- total-width layout-left-width layout-unaligned-width layout-right-width)))
           ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
           (cond
             [(and (false? last-word) (empty? remaining-words))
              ; final iteration
              ;; update the baseline position after all lines are placed
              ;(printf " final iteration~n")
              (set! layout-baseline-pos (+ ypos height))
              ;; add last line to layout element list
              (set! layout-unaligned-elements (cons (car lines) layout-unaligned-elements))
              (set! layout-unaligned-width (+ layout-unaligned-width (wrapped-line-w (car lines))))
              ;(printf " end line width=~a, luw=~a~n" (wrapped-line-w (car lines)) layout-unaligned-width)
              ;; set the element's lines field and put the lines in order
              (set-element-lines! e (reverse lines))
              (values x y (+ x max-width) (+ ypos font-height))]
             [(and (false? last-word) (empty? layout-unaligned-elements) (empty? layout-left-elements) (empty? layout-right-elements))
              ; first word is too long to fit on a line, just place it at the beginning of line
              ;(printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
              (define xpos 0)
              (define new-ypos ypos)
              (when (not (empty? (cdr remaining-words)))
                (set! new-ypos (calc-next-line-y ypos layout-unaligned-elements))
                (layout-goto-new-line new-ypos))
              (loop (cdr remaining-words)
                    (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                    (word-end-pos (car remaining-words))
                    new-ypos
                    (min xpos x)
                    (max width max-width))]
             [(false? last-word)
              ; first word is too long, advance line and try again
              (define new-ypos (calc-next-line-y ypos layout-unaligned-elements))
              (layout-goto-new-line new-ypos)
              (loop remaining-words
                    lines
                    line-start-pos
                    new-ypos
                    x
                    max-width)]
             [else
              (define xpos (+ layout-left-width layout-unaligned-width))
              (define new-ypos ypos)
              (when (not (empty? remaining-words))
                (set! new-ypos (calc-next-line-y ypos layout-unaligned-elements))
                (layout-goto-new-line new-ypos))
              (loop remaining-words
                    (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                    (word-end-pos last-word)
                    new-ypos
                    (min xpos x)
                    (max width max-width))]))]))

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
             ;(printf "layout center aligned element~n")
             ; we assume that center and unaligned elements are mutually exclusive on the same line
             ; the canvas user must end a line before adding an element with the other alignment
             ; otherwise, behavior is undefined
             (if (empty? layout-center-elements)
                 (let ([margin (/ (- total-width layout-left-width layout-right-width ew) 2)])
                   (set! layout-center-elements (cons e layout-center-elements))
                   (set! layout-center-width (+ ew snip-xmargin))
                   (set! layout-baseline-pos (+ y eh))
                   (values (+ layout-left-width margin) y (+ layout-left-width margin ew) (+ y eh)))
                 (let* ([margin (/ (- total-width layout-left-width layout-right-width layout-center-width ew) 2)]
                        [pos (+ layout-left-width margin layout-center-width)]
                        [y1 y]
                        [y2 (+ y eh)])
                   ; reposition each centered element on the line
                   (define old-margin ( / (- total-width layout-left-width layout-center-width layout-right-width) 2))
                   (define diff (- margin old-margin))
                   (adjust-elements-xpos! layout-center-elements diff)
                   (when (> y2 layout-baseline-pos)
                     (define diff (- y2 layout-baseline-pos))
                     (adjust-elements-ypos! layout-center-elements diff)
                     (set! layout-baseline-pos y2))
                   (when (< y2 layout-baseline-pos)
                     ; adjust y position to touch the baseline
                     (define diff (- layout-baseline-pos y2))
                     (set! y1 (+ y1 diff))
                     (set! y2 (+ y2 diff)))
                   (set! layout-center-elements (cons e layout-center-elements))
                   (set! layout-center-width (+ layout-center-width ew snip-xmargin))
                   (values pos y1 (+ pos ew) y2)))]
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

    (define/public (reset-layout)
      ;(printf "resetting cell layout~n")
      (set! content-width 0)
      (set! content-height 0)
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
        (place-element e place-x place-y)))

    (define (handle-element-properties e)
      (define snip (element-snip e))
      (when (is-a? snip snip%)
        (for ([prop (in-list (element-properties e))])
          #;(printf "handle cell element property ~a~n" prop)
          (case (car prop)
            [(resizable)
             (define-values (dw dh) (get-drawable-size))
             (define space-available (- dw layout-left-width (unaligned-or-center-width) layout-right-width))
             (define w (* space-available (/ (cdr prop) 100.0)))
             (define h (get-element-height e))
             (send snip resize w h)]))))

    ;; places element in the cell and updates size of the cell's content
    ;; e is the new element and must be the new tail of the element list
    ;; x,y is position of element's upper left corner in canvas
    (define (place-element e x y)
      (define (element-is-vertical-whitespace? e)
        (define s (element-snip e))
        (and (string? s)
             (not (non-empty-string? (string-trim s "\n" #:repeat? #t)))))
      
      (define-values (dw dh) (get-drawable-size))
      
      ;; coordinates for element bounding region
      ;; x2, y2 need to be set below
      ;; x1, y1 may be changed below (in the case of the layout mode)
      (define-values (x1 y1 x2 y2) (values x y 0 0))

      ;; cause get-extent to recalculate text extents by deleting cached value
      (set-element-cached-text-extent! e #f)

      (handle-element-properties e)

      (if (string? (element-snip e))
          ;; if snip is actually a string type
          (begin
            ;; calculate the extent of individual words
            ;; only have to do this once
            (when (not (element-words e))
              (calc-word-extents e))
            (set!-values (x1 y1 x2 y2) (layout-string e dw y))
            (set-element-cached-text-extent! e (text-extent (- x2 x1) (- y2 y1) 0 0)) 
            ;(printf "layout placed ~a (~a,~a)-(~a,~a) left:~a, una:~a, right:~a~n" (element-alignment e) x1 y1 x2 y2 layout-left-width layout-unaligned-width layout-right-width)
            (printf "layout placed ~a string at (~a,~a)-(~a,~a), cw=~a, xmargin=~a~n" (element-alignment e) x1 y1 x2 y2 cell-width xmargin)
            ;; set position for adding next element
            (when (element-end-of-line e)
              (layout-goto-new-line (next-line-y-pos y1))))
          ;; if an actual snip%
          (let ([snip-w (box 0)]
                [snip-h (box 0)]
                [snip-descent (box 0)]
                [snip-space (box 0)])
            (get-extent e dc x y snip-w snip-h snip-descent snip-space #f #f)
            (define snip-height (if (is-a? (element-snip e) string-snip%)
                                    (- (unbox snip-h) (unbox snip-descent))
                                    (unbox snip-h)))
            (set!-values (x1 y1 x2 y2) (layout-snip e dw y (unbox snip-w) snip-height))
            ;(printf "layout placed ~a (~a,~a)-(~a,~a) left:~a, una:~a, right:~a~n" (element-alignment e) x1 y1 x2 y2 layout-left-width layout-unaligned-width layout-right-width)
            ;(printf "extent: ~ax~a~n" (unbox snip-w) (unbox snip-h))
            ; layout-goto-new-line needs the element's position to be set, so set it early for now
            (set-element-xpos! e x1)
            (set-element-ypos! e y1)
            ; set position for adding next element
            ; should we allow left or right aligned elements be marked as end-of-line?
            (when (element-end-of-line e)
              ;(printf "snip end of line~n")
              (layout-goto-new-line (next-line-y-pos y1)))))

      ;(printf "element bb = (~a,~a) (~a,~a)~n" x1 y1 x2 y2)

      ;; set the element's position
      (set-element-xpos! e x1)
      (set-element-ypos! e y1)

      (when (> x2 content-width)
        (set! content-width x2))
      
      ;; don't adjust content height if element is just vertical whitespace
      ;; trailing vertical whitespace breaks vertical alignment of content
      (when (and (> y2 content-height) (not (element-is-vertical-whitespace? e)))
        (set! content-height y2)))

    ;; during first pass of adding elements we will calculate the min and max width of the cell
    (define min-width 0)
    (define max-width 0)

    (define/public (get-min-width)
      (define auto-min-width (+ min-width (* 2 xmargin)))
      auto-min-width
      ;(define fw (+ fixed-width (* 2 xmargin)))
      #;(if (> fw 0)
          (if (> fw auto-min-width) fw auto-min-width)
          auto-min-width))

    (define/public (get-max-width)
      (define auto-max-width (+ max-width (* 2 ymargin)))
      auto-max-width
      ;(define fw (+ fixed-width (* 2 xmargin)))
      #;(if (> fw 0)
          (if (> auto-max-width fw) auto-max-width fw)
          auto-max-width))
    
    (define (initial-place-element e x y)
      (define (unnecessary-margin)
        ; TODO: this takes care of the most common cases, but not all
        (or
         (and (> layout-left-width 0) (= 0 (+ (unaligned-or-center-width) layout-right-width)) snip-xmargin)
         (and (> (unaligned-or-center-width) 0) (= layout-right-width 0) snip-xmargin)
         0))
      
      ;; coordinates for element bounding region
      ;; x2, y2 need to be set below
      ;; x1, y1 may be changed below
      (define-values (x1 y1 x2 y2) (values x y 0 0))

      ;; for initial pass assume a large drawable width. ideally we wouldn't check the width
      ;; at all during layout but that would require changing the layout algorithm a bit,
      ;; so do this for now.
      (define dw 10000)
      
      ;; for strings calculate word extents. only need to do this once for each string element.
      ;; minimum width of the cell is equal to width of shortest word or smallest snip
      (when (string? (element-snip e))
        (calc-word-extents e)
        (for ([w (in-list (element-words e))])
          ;(printf "word width = ~a~n" (word-width w))
          (when (> (word-width w) min-width)
            (set! min-width (word-width w)))))
      
      ;; layout the element without word wrapping, so treat strings and snips the same
      (let ([snip-w (box 0)]
            [snip-h (box 0)]
            [snip-descent (box 0)]
            [snip-space (box 0)])
        (get-extent e dc x y snip-w snip-h snip-descent snip-space #f #f)
        (define snip-height (if (or (string? (element-snip e))
                                    (is-a? (element-snip e) string-snip%))
                                (- (unbox snip-h) (unbox snip-descent))
                                (unbox snip-h)))
        ; update min width for non-strings
        (when (and (not (string? (element-snip e)))
                   (> (unbox snip-w) min-width))
          #;(printf "set min width to ~a~n" (unbox snip-w))
          (set! min-width (unbox snip-w)))
        (set!-values (x1 y1 x2 y2) (layout-snip e dw y (unbox snip-w) snip-height))
        ; layout-goto-new-line needs the element's position to be set, so set it now
        (set-element-xpos! e x1)
        (set-element-ypos! e y1)
        ; calculate line width before advancing to next line
        (define line-width (- (+ layout-left-width (unaligned-or-center-width) layout-right-width)
                              (unnecessary-margin)))
        (when (> line-width max-width)
          (set! max-width line-width))
        ; set position for adding next element
        (when (element-end-of-line e)
          ;(printf "snip end of line~n")
          (layout-goto-new-line (next-line-y-pos y1)))))
    
    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      #;(printf "cell append-snip~n")
      (define e (element s end-of-line alignment properties))
      (initial-place-element e place-x place-y)
      (dlist-append! elements e))

    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned])
      #;(printf "cell append-string ~a~n" s)
      (define e (element s end-of-line alignment '()))
      (set-element-text-style! e (or style default-style))
      (initial-place-element e place-x place-y)
      (dlist-append! elements e))

    (define (find-last-element [alignments '(unaligned center)])
      ;; the end-of-line flag is only relevant for unaligned or centered elements
      (for/or ([e (in-dlist-reverse elements)]
               #:when (memq (element-alignment e) alignments))
        e))

    (define/public (last-element-eol?)
      (define last-element (find-last-element))
      (if last-element
          (element-end-of-line last-element)
          ;; true if no elements
          #t))

    ;; last element ends with whitespace
    (define/public (last-element-ews?)
      (define last-element (find-last-element))
      (if last-element
          (or (and (string? (element-snip last-element)) (string-suffix? (element-snip last-element) " "))
              (element-end-of-line last-element))
          #f))

    (define/public (set-last-element-eol)
      (define last-element (find-last-element))
      (when (and last-element (not (element-end-of-line last-element)))
        (set-element-end-of-line! last-element #t)
        (layout-goto-new-line (next-line-y-pos (element-ypos last-element)))))

))

(define table-snip%
  (class snip% (super-new)

    (init drawing-context
          defstyle
          [border 0]
          [cellspacing 2]
          [cellpadding 1]
          [rules 'all]
          [w #f])

    (define dc drawing-context)
    (define default-style defstyle)
    (define cell-inner-margin cellpadding)

    (define border-line-width border)
    (define border-width (+ border cellspacing))
    (define cell-border-line-width border)
    (define column-rule-width
      (if (or (eq? rules 'all) (eq? rules 'cols))
          cellspacing
          0))
    (define row-rule-height
      (if (or (eq? rules 'all) (eq? rules 'rows))
          cellspacing
          0))

    ;; either #f, a value in pixels, or a value between 0 and 1 to represent a width in percent
    (define desired-table-width w)
    
    (define num-rows 0)
    (define num-columns 0)
    (define width 0)
    (define height 0)
    (define min-width 0)
    (define max-width 0)

    ;; first row of table's upper left corner in canvas coordinates
    ;; set when draw function is called by canvas
    (define first-row-x 0)
    (define first-row-y 0)
    (define (get-first-row-position)
      (values first-row-x first-row-y))

    (define (set-first-row-position posx posy)
      (set! first-row-x posx)
      (set! first-row-y posy))

    (define (table-position-changed? x y)
      (or (not (= x first-row-x))
          (not (= y first-row-y))))
    
    (define/override (get-extent dc x y	 	 	 	 
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (maybe-set-box! w width)
      (maybe-set-box! h height)
      (maybe-set-box! descent 0.0)
      (maybe-set-box! space 0.0)
      (maybe-set-box! lspace 0.0)
      (maybe-set-box! rspace 0.0))

    (define/override (resize w h)
      (printf "resize table to width ~a~n" w)
      ;(calc-column-min/max-widths)
      (set-column-widths (- w (table-border-rule-width)))
      (calc/set-table-size))
    
    (define (calc-lit-color dc)
      (define bg-color (send dc get-background))
      (make-object color% #xe8 #xe8 #xe8))

    (define (calc-shadow-color dc)
      (define bg-color (send dc get-background))
      (make-object color% #x72 #x72 #x72))
    
    (define (draw-table-border dc x y w h thickness)
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      (define old-brush (send dc get-brush))

      (define x2 (sub1 (+ x w)))
      (define y2 (sub1 (+ y h)))
      
      (send dc set-smoothing 'aligned)
      ; left and top edges are lit (brighter color)
      (send dc set-pen (calc-lit-color dc) thickness 'solid)
      (send dc draw-line x y2 x y)
      (send dc draw-line x y x2 y)
      ; bottom and right edges are in shadow (darker color)
      (send dc set-pen (calc-shadow-color dc) thickness 'solid)
      (send dc draw-line x2 y x2 y2)
      (send dc draw-line x y2 x2 y2)
      
      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen)
      (send dc set-brush old-brush))

    (define (draw-cell-border dc x y w h thickness)
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      (define old-brush (send dc get-brush))

      (define x2 (sub1 (+ x w)))
      (define y2 (sub1 (+ y h)))
      
      (send dc set-smoothing 'aligned)
      ; left and top edges are in shadow (darker color)
      (send dc set-pen (calc-shadow-color dc) thickness 'solid)
      (send dc draw-line x y2 x y)
      (send dc draw-line x y x2 y)
      ; bottom and right edges are lit (brighter color)
      (send dc set-pen (calc-lit-color dc) thickness 'solid)
      (send dc draw-line x2 y x2 y2)
      (send dc draw-line x y2 x2 y2)
      
      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen)
      (send dc set-brush old-brush))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define startx (+ x border-width))
      (define ypos (+ y border-width))
      (define xpos-liminal-space
        (if (> column-rule-width 0)
            (+ (* cell-border-line-width 2) column-rule-width)
            0))
      (define ypos-liminal-space
        (if (> row-rule-height 0)
            (+ (* cell-border-line-width 2) row-rule-height)
            0))
      (define x-cell-border-width
        (if (> column-rule-width 0)
            cell-border-line-width
            0))
      (define y-cell-border-width
        (if (> row-rule-height 0)
            cell-border-line-width
            0))
      
      (printf "drawing table at ~ax~a~n" x y)

      ; if table position on canvas changes, update canvas coordinates of each cell
      (when (table-position-changed? startx ypos)
        (printf "table position changed~n")
        (set-first-row-position startx ypos)
        (for/fold ([rowy ypos])
                  ([row (in-list rows)])
          (for/fold ([rowx startx])
                    ([c (in-list row)])
            (send c set-position rowx rowy)
            (+ rowx (send c get-width) xpos-liminal-space))
          (+ rowy (row-height row) ypos-liminal-space)))
      
      (when (> border-line-width 0)
        (draw-table-border dc x y width height border-line-width))
      (for ([row (in-list rows)])
        (define xpos startx)
        ; only draw visible rows
        (when (and (>= (+ ypos (row-height row)) top)
                   (<= ypos bottom))
          (for ([c (in-list row)])
            (define cwidth (send c get-width))
            (define cheight (send c get-height))
            (when (and (> column-rule-width 0) (> row-rule-height 0))
              (draw-cell-border dc
                                xpos
                                ypos
                                (+ cwidth (* cell-border-line-width 2))
                                (+ cheight (* cell-border-line-width 2))
                                cell-border-line-width))
            (send c draw dc (+ xpos x-cell-border-width) (+ ypos y-cell-border-width) left top right bottom dx dy)
            (set! xpos (+ xpos cwidth xpos-liminal-space))))
        (set! ypos (+ ypos (row-height row) ypos-liminal-space))))

    (define (select-cell x y)
      (for/or ([row (in-list rows)])
        (for/or ([c (in-list row)])
          (define-values (cx cy) (send c get-position))
          (define-values (cw ch) (send c get-size))
          (and (>= x cx)
               (<= x (+ cx cw))
               (>= y cy)
               (<= y (+ cy ch))
               c))))

    (define cell-with-focus #f)

    (define/override (on-event dc x y editorx editory event)
      (case (send event get-event-type)
        [(left-down middle-down right-down motion)
         (define-values (ex ey) (values (send event get-x)
                                        (send event get-y)))
         (define-values (tx ty) (values (- ex x)
                                        (- ey y)))
         ;(printf "on-event: ~ax~a - ~ax~a = ~ax~a~n" ex ey x y tx ty)
         ; use raw canvas coordinates rather than coords relative to the table's position to
         ; select cell. each cell will save its raw canvas coordinates when drawn.
         (define c (select-cell ex ey))
         (when (and cell-with-focus (not (eq? c cell-with-focus)))
           (send cell-with-focus leave-cell dc ex ey event))
         (unless (eq? c cell-with-focus)
           (set! cell-with-focus c))
         (when c
           (send c on-event dc ex ey event))]
        [else
         void]))
    
    (define (table-border-rule-width)
      (+ (* 2 border-width) (* (* 2 cell-border-line-width) num-columns) (* column-rule-width (sub1 num-columns))))

    (define (table-border-rule-height)
      (+ (* 2 border-width) (* (* 2 cell-border-line-width) num-rows) (* row-rule-height (sub1 num-rows))))

    (define (calc/set-table-size)
      (define columns-width
        (for/fold ([total-width 0])
                  ([col (in-gvector columns)])
          (+ total-width (column-width col))))
      (define rows-height
        (for/fold ([total-height 0])
                  ([row (in-list rows)])
          (define h (row-height row))
          (+ total-height h)))
      (set! width (+ columns-width (* border-width 2) (* 2 cell-border-line-width num-columns) (* column-rule-width (sub1 num-columns))))
      (set! height (+ rows-height (* border-width 2) (* 2 cell-border-line-width num-rows) (* row-rule-height (sub1 num-rows))))
      (printf "table size is ~ax~a~n" width height))
    
    (struct column
      ([min-width #:mutable]
       [max-width  #:mutable]
       [cells  #:mutable] ; list of cells but not necessarily in row order
       [width #:mutable #:auto]
       [fixed-width  #:mutable #:auto]
       [relative-width  #:mutable #:auto])
      #:prefab #:auto-value 0)

    (define rows '())
    ;; growable vector of column structs
    (define columns (make-gvector))
    ;; row in progress
    (define rip #f)

    (define (current-cell)
      (and rip (car rip)))

    ;; adds each cell in row to its respective column
    ;; row is a list of cells in column order
    (define (add-row-to-columns row)
      (when (not (empty? row))
        (let loop ([cell (car row)]
                   [i 0]
                   [row (cdr row)])
          (define colspan (send cell get-colspan))
          ; when a cell spans multiple columns, add it to each column
          (for ([j (in-range 0 colspan)])
            (define column-index (+ i j))
            ; init a new column
            (when (= column-index (gvector-count columns))
              (gvector-add! columns (column 0 0 '())))
            (define col (gvector-ref columns column-index))
            (set-column-cells! col (cons cell (column-cells col))))
          (when (not (empty? row))
            (loop (car row)
                  (+ i colspan)
                  (cdr row))))))

    (define (calc-column-relative-width relative-width table-width)
      (if (= relative-width 0)
          0
          (let ([w (* table-width relative-width)]
                [margin (* 2 cell-inner-margin)])
            (if (< margin w)
                (- w margin)
                margin))))

    (define (calc-column-min/max-widths)
      (define (cell-min-width c)
        (define colspan (send c get-colspan))
        (floor (/ (send c get-min-width) colspan)))

      (define (cell-max-width c)
        (define colspan (send c get-colspan))
        (floor (/ (send c get-max-width) colspan)))

      (define (cell-fixed-width c)
        (define colspan (send c get-colspan))
        (floor (/ (send c get-fixed-width) colspan)))

      (define (cell-relative-width c)
        (define colspan (send c get-colspan))
        (/ (send c get-relative-width) colspan))
      
      ;; calculate the min and max width of each column
      ;; also determine if the column should have a fixed width
      (for* ([col (in-gvector columns)]
             [c (in-list (column-cells col))])
        #;(printf "cell min/max/fixed = ~a/~a/~a~n" (cell-min-width c) (cell-max-width c) (cell-fixed-width c))
        (when (> (cell-min-width c) (column-min-width col))
          (set-column-min-width! col (cell-min-width c)))
        (when (> (cell-max-width c) (column-max-width col))
          (set-column-max-width! col (cell-max-width c)))
        (when (> (cell-fixed-width c) (column-fixed-width col))
          (set-column-fixed-width! col (cell-fixed-width c)))
        (when (> (cell-relative-width c) (column-relative-width col))
          (set-column-relative-width! col (cell-relative-width c)))
        #;(printf "calc-column-min/max-widths: ~a - ~a~n" (column-min-width col) (column-max-width col)))
      ;; calculate the min and max width of the table
      (define-values (table-min-width table-max-width)
        (for/fold ([t-min-width 0]
                   [t-max-width 0])
                  ([col (in-gvector columns)])
          (values (+ t-min-width (column-min-width col))
                  (+ t-max-width (column-max-width col)))))
      (set! min-width table-min-width)
      (set! max-width table-max-width))

    (define (row-height row)
      (if (empty? row)
          0
          (send (car row) get-height)))
    
    (define (calc-row-height row)
      (define max-height 0)
      (for ([c (in-list row)])
        (define-values  (w h) (send c get-content-size))
        (when (> h max-height)
          (set! max-height h)))
      max-height)

    (define (set-row-height row height)
      (for ([c (in-list row)])
        (send c set-height height)))
    
    (define (reset-column-widths)
      (for ([col (in-gvector columns)])
        (set-column-width! col 0)))

    (define (set-column-widths layout-width)
      (define desired-width
        (and desired-table-width
             (if (> desired-table-width 1)
                 (- desired-table-width (table-border-rule-width))
                 (* layout-width desired-table-width))))
      
      ;; algorithm relies on knowing which columns have had a width set on this pass,
      ;; so discard previous width calc results
      (reset-column-widths)
      
      (if desired-width
          ;; calculate the width of each column using fixed width algorithm
          (let ([default-column-width (/ desired-width num-columns)]
                [remaining-width desired-width]
                [remaining-columns num-columns])
            ; set width of columns with a specified width
            #;(printf "set fixed width columns~n")
            (for ([col (in-gvector columns)])
              (define w (column-fixed-width col))
              (when (> w 0)
                (when (< w (column-min-width col))
                  #;(printf "  column fixed width < min width, setting to min width~n")
                  (set! w (column-min-width col)))
                #;(printf "  setting column width to ~a~n" w)
                (set-column-width! col w)
                (set! remaining-columns (sub1 remaining-columns))
                (set! remaining-width (- remaining-width w))))
            ; set width of columns with a relative width
            (for ([col (in-gvector columns)])
              (define rel-w (column-relative-width col))
              (when (and (> rel-w 0) (= (column-fixed-width col) 0))
                (define w (calc-column-relative-width rel-w desired-width))
                (cond
                  [(< w (column-min-width col))
                   (printf "  column relative width < min width, skipping~n")]
                  [(>= w remaining-width)
                   (printf "  column relative width > available width, skipping~n")]
                  [else
                   (printf "  setting column with relative width ~a to width ~a~n" rel-w w)
                   (set-column-width! col w)
                   (set! remaining-columns (sub1 remaining-columns))
                   (set! remaining-width (- remaining-width w))])))
            ; divide the remaining space evenly among the rest of the columns
            (when (> remaining-columns 0)
              (define w (/ remaining-width remaining-columns))
              (printf "divide remaining space of ~a, cols=~a, w=~a~n" remaining-width remaining-columns w)
              (for ([col (in-gvector columns)])
                (when (= (column-width col) 0)
                  (if (>= w (column-min-width col))
                      (begin
                        #;(printf "  setting column width to ~a~n" w)
                        (set! remaining-width (- remaining-width w))
                        (set-column-width! col w))
                      (begin
                        #;(printf "  setting column width to min width ~a~n" (column-min-width col))
                        (set! remaining-width (- remaining-width (column-min-width col)))
                        (set-column-width! col (column-min-width col)))))))
            ;; if table exceeds desired width, try to shrink columns
            (when (< remaining-width 0)
              (printf "table exceeds desired width by ~a, try to shrink some columns~n" (abs remaining-width))
              (define column-diffs (make-vector num-columns 0))
              (for ([col (in-gvector columns)]
                    [i (in-naturals 0)])
                #;(printf "  column ~a diff is ~a-~a~n" i (column-width col) (column-min-width col))
                (vector-set! column-diffs i (cons i (- (column-width col) (column-min-width col)))))
              (let loop ([diff (abs remaining-width)])
                (cond
                  [(<= diff 0) void]
                  [else
                   (define next-largest-diff (vector-argmax cdr column-diffs))
                   (printf "  next largest diff=~a (~a still needed) ~n" next-largest-diff diff)
                   (if (> (cdr next-largest-diff) 0)
                       (let ([col (gvector-ref columns (car next-largest-diff))]
                             [index (car next-largest-diff)]
                             [delta (min diff (cdr next-largest-diff))])
                         (printf "  shrink column ~a by ~a~n" index delta)
                         (set-column-width! col (- (column-width col) delta))
                         (vector-set! column-diffs index (cons index 0))
                         (loop (- diff delta)))
                       (printf "  no more space available~n"))]))))
          ;; calculate the width of each column using autolayout algorithm
          (let ([rem-layout-width layout-width]
                [rem-max-width max-width]
                [rem-min-width min-width])
            (printf "running auto layout algorithm~n")
            ;; first set any fixed width columns
            (for ([col (in-gvector columns)])
              (define fw (column-fixed-width col))
              (when (and (> fw 0)
                         (>= fw (column-min-width col)))
                #;(printf "  set column to fixed width ~a~n" fw)
                (set-column-width! col fw)
                (set! rem-layout-width (- rem-layout-width fw))
                (set! rem-max-width (- rem-max-width (column-max-width col)))
                (set! rem-min-width (- rem-min-width (column-min-width col)))))
            ;; next set any relative width columns
            (for ([col (in-gvector columns)])
              (define rel-w (column-relative-width col))
              (when (and (> rel-w 0) (= (column-fixed-width col) 0))
                (define w (calc-column-relative-width rel-w layout-width))
                (cond
                  [(< w (column-min-width col))
                   (printf "  column relative width < min width, skipping~n")]
                  [(>= w rem-layout-width)
                   (printf "  column relative width > available width, skipping~n")]
                  [else
                   (printf "  setting column with relative width ~a to width ~a~n" rel-w w)
                   (set-column-width! col w)
                   (set! rem-layout-width (- rem-layout-width w))
                   (set! rem-max-width (- rem-max-width (column-max-width col)))
                   (set! rem-min-width (- rem-min-width (column-min-width col)))])))
            (cond
              [(<= rem-max-width rem-layout-width)
               ;; it all fits, set column widths to the max
               (for ([col (in-gvector columns)])
                 (when (= (column-width col) 0)
                   (set-column-width! col (column-max-width col))))]
              [(>= min-width layout-width)
               ;; min is still too big, set column widths to min including fixed width columns
               (for ([col (in-gvector columns)])
                 (set-column-width! col (column-min-width col)))]
              [(>= rem-min-width rem-layout-width)
               ;; as else case but recalculate fixed width columns
               (define W (- layout-width min-width))
               (define D (- max-width min-width))
               (define W-over-D (if (> D 0)
                                    (/ W D)
                                    0))
               (for ([col (in-gvector columns)])
                 (define d (- (column-max-width col) (column-min-width col)))
                 #;(printf "setting colmun width to ~a~n" (+ (column-min-width col) (floor (* d W-over-D))))
                 (set-column-width! col (+ (column-min-width col) (floor (* d W-over-D)))))]
              [else
               ;; assign column widths
               (define W (- rem-layout-width rem-min-width))
               (define D (- rem-max-width rem-min-width))
               (define W-over-D (if (> D 0)
                                    (/ W D)
                                    0))
               (for ([col (in-gvector columns)])
                 (when (= (column-width col) 0)
                   (define d (- (column-max-width col) (column-min-width col)))
                   (printf "setting colmun width to ~a~n" (+ (column-min-width col) (floor (* d W-over-D))))
                   (set-column-width! col (+ (column-min-width col) (floor (* d W-over-D))))))])))
      
      ;; set each cell's width to its column's width and run layout in each cell
      (define visited-cells (mutable-seteq))
      (for* ([col (in-gvector columns)]
             [c (in-list (column-cells col))])
        (if (set-member? visited-cells c)
            (begin
              #;(printf "spanning cell~n")
              (send c set-width (+ (send c get-width) (column-width col) column-rule-width (* 2 cell-border-line-width)))
              (send c reset-layout))
            (begin
              (send c set-width (column-width col))
              (send c reset-layout)
              (set-add! visited-cells c))))
      
      ;; now we should know the content height of each cell and can set the cells
      ;; in each row to a suitable height
      (for ([row (in-list rows)])
        (define height (calc-row-height row))
        #;(printf "setting row height to ~a~n" height)
        (set-row-height row height)))

    (define/public (start-row)
      (set! rip '()))

    (define/public (end-row)
      (define num-cols (length rip))
      ;; todo handle cells that span multiple columns
      (when (> num-cols num-columns)
        (set! num-columns num-cols))
      (set! num-rows (add1 num-rows))
      (set! rows (cons (reverse rip) rows))
      (add-row-to-columns (car rows))
      (set! rip #f))

    (define/public (start-cell #:colspan [colspan 1] #:valign [valign 'middle] #:bgcolor [bgcolor #f] #:width [width #f])
      (define fixed-width
        (and width (pair? width) (eq? (car width) 'width-pixels) (cdr width)))
      (define relative-width
        (and width (pair? width) (eq? (car width) 'width-percent) (/ (cdr width) 100)))
      (define c (new cell%
                     (drawing-context dc)
                     (defstyle default-style)
                     (colspan colspan)
                     (valign valign)
                     (bgcolor bgcolor)
                     (width (or fixed-width relative-width))
                     (horiz-margin cell-inner-margin)
                     (vert-margin cell-inner-margin)))
      (set! rip (cons c rip)))

    (define/public (end-cell)
      ;(printf "min/max width=~a/~a~n" (send (current-cell) get-min-width) (send (current-cell) get-max-width))
      void)

    (define/public (finalize-table layout-width)
      (set! rows (reverse rows))
      (printf "finalize table ~ax~a~n" num-columns num-rows)
      (calc-column-min/max-widths)
      (set-column-widths (- layout-width (table-border-rule-width)))
      (printf "table border+rule size = ~a~n" (table-border-rule-width))
      ;(printf "table rows are:~a~n" rows)
      #;(for ([row (in-list rows)]
            [i (in-naturals 0)])
        (printf "row ~a: " i)
        (for ([c (in-list row)])
          (define-values (w h) (send c get-size))
          (printf "~ax~a, " w h))
        (printf "~n"))
      (calc/set-table-size))
    
    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      (define c (current-cell))
      (when c
        (send c append-snip s end-of-line alignment properties)))

    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned])
      (define c (current-cell))
      (when c
        (send c append-string s style end-of-line alignment)))

    (define/public (last-element-eol?)
      (define c (current-cell))
      (when c
        (send c last-element-eol?)))

    (define/public (last-element-ews?)
      (define c (current-cell))
      (when c
        (send c last-element-ews?)))

    (define/public (set-last-element-eol)
      void)
    
    ))

(module+ main
  (require "layout-canvas.rkt")
  
  (define frame 
    (new (class frame% (super-new))
         [label "Table test"]
         [width 800]
         [height 600]))

  (define canvas
    (new layout-canvas% (parent frame)
         (horiz-margin 5)
         (vert-margin 5)))

  (send canvas set-canvas-background (make-color 200 200 200))

  (define standard-style
    (send (send canvas get-style-list) find-named-style "Standard"))
  
  (define table (new table-snip%
                     (drawing-context (send canvas get-dc))
                     (defstyle standard-style)
                     ;(cellpadding 5)
                     (w 120)
                     (border 1)))

  (send canvas append-string "There is a table below this line:" #f #t)
  
  (send table start-row)
  (send table start-cell #:bgcolor (make-color 200 0 0) #:width '(width-pixels . 9))
  (send table append-string "~" #f)
  (send table end-cell)
  (send table start-cell #:colspan 2)
  (send table append-string "1,2")
  (send table end-cell)
  (send table end-row)
  
  (send table start-row)
  (send table start-cell)
  (send table append-string "~")
  (send table end-cell)
  (send table start-cell)
  (send table append-string "2,2")
  (send table end-cell)
  (send table start-cell)
  (send table append-string "2,3")
  (send table end-cell)
  (send table end-row)

  (define-values (dw dh) (send canvas get-drawable-size))
  (send table finalize-table dw)
  
  (send canvas append-snip table)
  
  (send frame show #t))
