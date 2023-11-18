#lang racket/gui

(require racket/class
         racket/snip
         racket/draw)

(require "dlist.rkt")

(provide table-snip%)

(define cell%
  (class object% (super-new)
    (init drawing-context
          defstyle
          [horiz-margin 5]
          [vert-margin 5])

    (define dc drawing-context)
    (define default-style defstyle)

    ;; 
    (define xmargin horiz-margin)
    (define ymargin vert-margin)
    ;; upper left corner of cell in table's virtual canvas
    (define x 0)
    (define y 0)
    ;; size of cell not including borders, which are drawn by table
    (define width 10)
    (define height 10)

    (define snip-xmargin 5)
    (define snip-ymargin 2)

    ;; current position to place next element
    (define place-x 0)
    (define place-y 0)
    

    (define/public (get-dc)
      dc)

    (define/public (get-position)
      (values x y))

    (define/public (set-position posx posy)
      (set! x posx)
      (set! y posy))
    
    (define/public (get-size)
      (values width height))

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

    (define (draw-wrapped-text e dc left top)
      (for ([line (in-list (element-lines e))])
        ;(printf "draw-wrapped-text: (~a,~a) ~a~n" (wrapped-line-x line) (wrapped-line-y line) (substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line)))
        (send/apply dc draw-text `(,(substring (element-snip e) (wrapped-line-start-pos line) (wrapped-line-end-pos line)) ,(+ (- (wrapped-line-x line) left) xmargin) ,(+ (- (wrapped-line-y line) top) ymargin)))))
    
    (define (draw-element e dc x y left top right bottom dx dy)
      (if (string? (element-snip e))
          (send dc draw-text (element-snip e) x y)
          (send (element-snip e) draw dc x y left top right bottom dx dy 'no-caret)))

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

    (define (next-line-y-pos original)
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
          (printf "next-line-y-pos: all layout lists are empty~n")
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
      (define to-next-y (+ font-height snip-ymargin))
      
      (case (element-alignment e)
        [(right)
         (let loop ([words (element-words e)]
                    [lines '()]
                    [line-start-pos 0]
                    [ypos y]
                    [x (- total-width layout-right-width)]
                    [max-width 0])
           (printf "loop right: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
           (define-values (last-word width remaining-words)
             (layout-remainder-of-line words (- total-width layout-left-width (unaligned-or-center-width) layout-right-width)))
           ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
           (cond
             [(and (false? last-word) (empty? remaining-words))
              ; final iteration
              (printf " final iteration~n")
              ;; add last line to layout element list
              (set! layout-right-elements (cons (car lines) layout-right-elements))
              (set! layout-right-width (+ layout-right-width (wrapped-line-w (car lines))))
              ;(printf " end line width=~a, lrw=~a~n" (wrapped-line-w (car lines)) layout-right-width)
              ;; set the element's lines field and put the lines in order
              (set-element-lines! e (reverse lines))
              (values x y (+ x max-width) (+ ypos font-height))]
             [(and (false? last-word) (empty? (unaligned-or-center-elements)) (empty? layout-left-elements) (empty? layout-right-elements))
              ; first word is too long to fit on a line, just place it at the beginning of line
              (printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
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
         (define words (element-words e))
         (define lines '())
         (define max-width 0)
         (define x total-width) ; just default to something large and shrink as we add lines
         (define ypos y)
         ;; if there is a partial line of centered elements, finish the line and deal with the remaining words below
         (when (not (empty? layout-center-elements))
           (define-values (last-word width remaining-words)
             (layout-remainder-of-line words (- total-width layout-left-width layout-center-width layout-right-width)))
           (when (false? last-word)
             (set! ypos (next-line-y-pos y))
             (layout-goto-new-line ypos))
           (when last-word
             ;; shift existing elements over to make room
             (define old-margin ( / (- total-width layout-left-width layout-center-width layout-right-width) 2))
             (define margin ( / (- total-width layout-left-width layout-center-width layout-right-width width) 2))
             (define diff (- margin old-margin))
             ;(printf "shift centered elements over: lcw=~a, width =~a, margin ~a to ~a, diff=~a~n" layout-center-width width old-margin margin diff)
             (adjust-elements-xpos! layout-center-elements diff)
             ;;
             (set! x (+ layout-left-width margin layout-center-width))
             (set! lines (cons (wrapped-line 0 (word-end-pos last-word) x y width font-height) lines))
             (set! words remaining-words)
             (set! max-width width)
             (set! layout-center-width (+ layout-center-width width))
             (when (not (empty? remaining-words))
               (set! ypos (+ y to-next-y))
               (layout-goto-new-line ypos))))

         (define width 0)
         (define baseline (+ ypos height))

         ;(printf "space avail=~a, x=~a~n" space-available x)
         
         (if (> baseline layout-baseline-pos)
             (when (not (empty? layout-center-elements))
               (define diff (- baseline layout-baseline-pos))
               (adjust-elements-ypos! layout-center-elements diff))
             (let ([diff (- layout-baseline-pos baseline)])
               (set! ypos (+ ypos diff))))
         
         (when (not (empty? words))
           (define space-available (- total-width layout-left-width layout-right-width))
           (set! x (+ layout-left-width (/ space-available 2)))
           (define start-pos (word-pos (car words))) ; line's starting position (an index into the string)
           (for ([w (in-list words)])
             (if (< (+ width (word-to-next w)) space-available)
                 (set! width (+ width (word-to-next w)))
                 (let ([w-width (+ width (word-width w))])
                   ;; wrap to next line, but first check if w fits on the current line
                   (if (<=  w-width space-available)
                       (let* ([margin (/ (- space-available w-width) 2)]
                              [xpos (+ layout-left-width margin)])
                         (set! lines (cons (wrapped-line start-pos (word-end-pos w) xpos ypos w-width font-height) lines))
                         (set! start-pos (word-end-pos w))
                         (when (> w-width max-width)
                           (set! max-width w-width))
                         ;; advance to the new line
                         (set! ypos (+ ypos to-next-y))
                         (set! width 0)
                         (layout-goto-new-line ypos)
                         (when (< xpos x)
                           (set! x xpos)))
                       (let* ([margin (/ (- space-available width) 2)]
                              [xpos (+ layout-left-width margin)])
                         (set! lines (cons (wrapped-line start-pos (word-pos w) xpos ypos width font-height) lines))
                         (set! start-pos (word-pos w))
                         (when (> width max-width)
                           (set! max-width width))
                         ;; advance to the new line
                         (set! ypos (+ ypos to-next-y))
                         (set! width 0)
                         (layout-goto-new-line ypos)
                         (when (< xpos x)
                           (set! x xpos)))))))
           ;; add last line of element
           (when (< start-pos (string-length (element-snip e)))
             (define-values (last-line-width unused-h unused-d unused-s) (send dc get-text-extent (substring (element-snip e) start-pos) font))
             (when (> last-line-width max-width)
               (set! max-width last-line-width))
             (set! layout-center-width last-line-width)
             (define margin (/ (- space-available last-line-width) 2))
             (define xpos (+ layout-left-width margin))
             (when (< xpos x)
               (set! x xpos))
             ;(printf "adding last line of element (~a,~a) ~ax~a~n" xpos ypos last-line-width font-height)
             (set! lines (cons (wrapped-line start-pos (string-length (element-snip e)) xpos ypos last-line-width font-height) lines))))
         ;; update the baseline position after all lines are placed
         (set! layout-baseline-pos (+ ypos height))
         ;; add last line to layout list
         (set! layout-center-elements (cons (car lines) layout-center-elements))
         ;; set the element's lines field and put the lines in order
         (set-element-lines! e (reverse lines))
         (values x y (+ x max-width) (+ ypos font-height))]
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
              (define xpos (+ layout-left-width layout-unaligned-width))
              (define new-ypos ypos)
              (when (not (empty? remaining-words))
                (set! new-ypos (+ ypos to-next-y))
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

    (define (reset-layout)
      (printf "resetting layout~n")
      (set! width 10)
      (set! height 10)
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
        (define-values (dw dh) (get-drawable-size))
        (for ([prop (in-list (element-properties e))])
          (printf "handle element property ~a~n" prop)
          (case (car prop)
            [(width-percent)
             (define space-available (- dw layout-left-width (unaligned-or-center-width) layout-right-width))
             (define w (* space-available (/ (cdr prop) 100.0)))
             (define h (get-element-height e))
             (send snip resize w h)]
            [(width-pixels)
             (define w (cdr prop))
             (define h (get-element-height e))
             (send snip resize w h)]))))
    
    ;; places element in the cell and updates size of the cell (or the height anyways)
    ;; e is the new element and must be the new tail of the element list
    ;; x,y is position of element's upper left corner in canvas
    (define (place-element e x y)
      (define-values (dw dh) (get-drawable-size))
      
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
          (begin
            ;; calculate the extent of individual words
            ;; only have to do this once
            (when (not (element-words e))
              (calc-word-extents e))
            (set!-values (x1 y1 x2 y2) (layout-string e dw y))
            (set-element-cached-text-extent! e (text-extent (- x2 x1) (- y2 y1) 0 0)) 
            ;(printf "layout placed ~a (~a,~a)-(~a,~a) left:~a, una:~a, right:~a~n" (element-alignment e) x1 y1 x2 y2 layout-left-width layout-unaligned-width layout-right-width)
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

      ; for cells the width will be set by the table
      #;(when (> x2 canvas-width)
        (set! canvas-width x2))

      (when (> y2 height)
        (set! height y2)))

    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      (printf "cell append-snip~n"))

    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned])
      (printf "cell append-string~n"))
))

(define table-snip%
  (class snip% (super-new)

    (init drawing-context
          defstyle)

    (define dc drawing-context)
    (define default-style defstyle)
    
    (define num-rows 0)
    (define num-columns 0)
    (define min-width 0)
    (define max-width 0)

    ; temp variables used for calculation of min and max cell width
    (define min-left 0)
    (define max-left 0)
    (define min-right 0)
    (define max-right 0)
    (define min-unaligned 0)
    (define max-unaligned 0)
    
    (define rows '())
    ;; row in progress
    (define rip #f)

    (define (current-cell)
      (and rip (car rip)))
    
    (define/public (start-row)
      (set! rip '()))

    (define/public (end-row)
      (define num-cols (length rip))
      ;; todo handle cells that span multiple columns
      (when (> num-cols num-columns)
        (set! num-columns num-cols))
      (set! num-rows (add1 num-rows))
      (set! rows (cons (reverse rip) rows))
      (set! rip #f))

    (define/public (start-cell)
      (define c (new cell% (drawing-context dc) (defstyle default-style)))
      (set! rip (cons c rip)))

    (define/public (end-cell)
      void)

    (define/public (finalize-table)
      (set! rows (reverse rows))
      (printf "finalize table ~ax~a, ~a~n" num-rows num-columns rows))

    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      (when (current-cell)
        (define c (car rip))
        (send c append-snip s end-of-line alignment properties)))

    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned])
      (when (current-cell)
        (define c (car rip))
        (send c append-string s style end-of-line alignment)))

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

  (send canvas set-canvas-background (make-color 33 33 33))

  (define standard-style
    (send (send canvas get-style-list) find-named-style "Standard"))
  
  (define table (new table-snip% (drawing-context (send canvas get-dc))
                                 (defstyle standard-style)))

  (send table start-row)
  (send table start-cell)
  (send table end-cell)
  (send table start-cell)
  (send table end-cell)
  (send table end-row)
  
  (send table start-row)
  (send table start-cell)
  (send table end-cell)
  (send table start-cell)
  (send table end-cell)
  (send table start-cell)
  (send table end-cell)
  (send table end-row)

  (send table finalize-table)
  
  (send canvas append-snip table)
  
  (send frame show #t))