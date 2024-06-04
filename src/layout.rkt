#lang racket/gui

(provide
 (struct-out text-extent)
 (struct-out word)
 (struct-out wrapped-line)
 (struct-out element)
 (struct-out layout-context)
 describe-element
 new-layout-context
 get-extent
 get-element-width
 get-element-height
 get-element-x
 get-element-y
 get-style
 calc-word-extents
 layout-advance-to-new-line
 unaligned-or-center-width
 unaligned-or-center-elements
 layout-string
 layout-snip)

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

;; state that needs to be maintained while adding elements in layout mode
(struct layout-context
  ([dc]
   [default-style #:mutable]
   [snip-xmargin]
   [snip-ymargin]
   [left-elements #:mutable]
   [right-elements #:mutable]
   [unaligned-elements #:mutable]
   [center-elements #:mutable]
   [left-width #:mutable #:auto]
   [right-width #:mutable #:auto]
   [unaligned-width #:mutable #:auto]
   [center-width #:mutable #:auto]
   [baseline-pos #:mutable #:auto]
   [place-x #:mutable #:auto]
   [place-y #:mutable #:auto])
  #:prefab #:auto-value 0)

(define (new-layout-context dc default-style snip-xmargin snip-ymargin)
  (layout-context dc default-style snip-xmargin snip-ymargin '() '() '() '()))

(define (describe-element e)
  (define (describe-string s)
    (define len (string-length s))
    (if (> len 20)
        (substring s 0 20)
        (substring s 0 len)))
  
  (if e
      (if (string? (element-snip e))
          (describe-string (element-snip e))
          (~a (element-snip e)))
      "#f"))

(define (get-extent ctx e x y [w #f] [h #f] [descent #f] [space #f] [lspace #f] [rspace #f])
  (define dc (layout-context-dc ctx))
  (if (string? (element-snip e))
      (if (element-cached-text-extent e)
          (let ([extent (element-cached-text-extent e)])
            (when w (set-box! w (text-extent-w extent)))
            (when h (set-box! h (text-extent-h extent)))
            (when descent (set-box! descent (text-extent-descent extent)))
            (when space (set-box! space (text-extent-space extent))))
          (let ([style (or (element-text-style e) (layout-context-default-style ctx))])
            (define-values (tw th td ts) (send dc get-text-extent (element-snip e) (send style get-font)))
            (when w (set-box! w tw))
            (when h (set-box! h th))
            (when descent (set-box! descent td))
            (when space (set-box! space ts))
            (set-element-cached-text-extent! e (text-extent tw th td ts))))
      (send (element-snip e) get-extent dc x y w h descent space lspace rspace)))

(define (get-element-width ctx e)
  (if (wrapped-line? e)
      (wrapped-line-w e)
      (let ([w (box 0)])
        (get-extent ctx e (element-xpos e) (element-ypos e) w)
        (unbox w))))

(define (get-element-height ctx e)
  (if (wrapped-line? e)
      (wrapped-line-h e)
      (let ([h (box 0)])
        (get-extent ctx e (element-xpos e) (element-ypos e) #f h)
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

(define (calc-word-extents ctx e)
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

  (define dc (layout-context-dc ctx))
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

;;;; HELPER FUNCTIONS
;;;; ------------------------------------------------------------
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

;; pop elements from the list of elements 'll' that don't extend to the new y value
;; 'lwidth' is the width of the items in 'll'
;; stop at first element that does extend to the new y value
;; returns the new list and the width in pixels of the items in that list
(define (pop-layout-list ctx ll lwidth new-y)
  ;(printf "pop-layout-list~n")
  (if (empty? ll)
      (values ll lwidth)
      (let ([e (car ll)])
        (let ([w (get-element-width ctx e)]
              [h (get-element-height ctx e)])
          (if (< (+ (get-element-y e) h)
                 new-y)
              (pop-layout-list ctx (cdr ll) (- lwidth w (if (wrapped-line? e) 0 (layout-context-snip-xmargin ctx))) new-y)
              (values ll lwidth))))))

(define (bottom-edge-of-elements ctx ll)
  (if (not (empty? ll))
      (+ (get-element-y (car ll))
         (get-element-height ctx (car ll)))
      (error "empty list")))

;; center and unaligned elements should be considered to all be on one line
;; the bottom edge is the bottom edge of the entire line and not just the
;; last element.
(define (bottom-edge-of-line ctx ll)
  (apply max (map (lambda (e) (+ (get-element-y e)
                                 (get-element-height ctx e)))
                  ll)))

(define (next-line-y-pos ctx original)
  (+ (layout-context-snip-ymargin ctx)
     (cond
       [(not (empty? (layout-context-unaligned-elements ctx)))
        ;(printf "beol unaligned ~a~n" (bottom-edge-of-line ctx (layout-context-unaligned-elements ctx)))
        (bottom-edge-of-line ctx (layout-context-unaligned-elements ctx))]
       [(not (empty? (layout-context-center-elements ctx)))
        (bottom-edge-of-line ctx (layout-context-center-elements ctx))]
       [(not (empty? (layout-context-left-elements ctx)))
        (if (empty? (layout-context-right-elements ctx))
            (bottom-edge-of-elements ctx (layout-context-left-elements ctx))
            (min (bottom-edge-of-elements ctx (layout-context-left-elements ctx))
                 (bottom-edge-of-elements ctx (layout-context-right-elements ctx))))]
       [(not (empty? (layout-context-right-elements ctx)))
        (bottom-edge-of-elements ctx (layout-context-right-elements ctx))]
       [else
        ; return original
        (printf "next-line-y-pos: all layout lists are empty~n")
        (- original (layout-context-snip-ymargin ctx))])))

(define (layout-goto-new-line ctx new-y)
  ;(printf "layout-goto-new-line: ~a~n" new-y)
  (set-layout-context-place-x! ctx 0) ; place-x value isn't currently used in layout mode
  (set-layout-context-place-y! ctx new-y)
  (set-layout-context-baseline-pos! ctx new-y)
  (define tmp-list '())
  (define tmp-width 0)
  (set!-values (tmp-list tmp-width) (pop-layout-list ctx (layout-context-left-elements ctx) (layout-context-left-width ctx) new-y))
  (set-layout-context-left-elements! ctx tmp-list)
  (set-layout-context-left-width! ctx tmp-width)
  (set!-values (tmp-list tmp-width) (pop-layout-list ctx (layout-context-right-elements ctx) (layout-context-right-width ctx) new-y))
  (set-layout-context-right-elements! ctx tmp-list)
  (set-layout-context-right-width! ctx tmp-width)
  (set!-values (tmp-list tmp-width) (pop-layout-list ctx (layout-context-center-elements ctx) (layout-context-center-width ctx) new-y))
  (set-layout-context-center-elements! ctx tmp-list)
  (set-layout-context-center-width! ctx tmp-width)
  ;(printf "before pop: ~a ~a~n" (layout-context-unaligned-elements ctx) (layout-context-unaligned-width ctx))
  (set!-values (tmp-list tmp-width) (pop-layout-list ctx (layout-context-unaligned-elements ctx) (layout-context-unaligned-width ctx) new-y))
  (set-layout-context-unaligned-elements! ctx tmp-list)
  (set-layout-context-unaligned-width! ctx tmp-width))
;;;; --------------------------------------------------------------

(define (layout-advance-to-new-line ctx cur-y)
  (layout-goto-new-line ctx (next-line-y-pos ctx cur-y)))

(define (unaligned-or-center-width ctx)
  (cond
    [(> (layout-context-unaligned-width ctx) 0) (layout-context-unaligned-width ctx)]
    [(> (layout-context-center-width ctx) 0) (layout-context-center-width ctx)]
    [else 0]))

(define (unaligned-or-center-elements ctx)
  (if (not (empty? (layout-context-center-elements ctx)))
      (layout-context-center-elements ctx)
      (layout-context-unaligned-elements ctx)))

(define (layout-string ctx e total-width y)
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
  (define-values (font-width font-height font-descent font-space) (send (layout-context-dc ctx) get-text-extent "a" font)) ; only need height, so string doesn't matter
  (define height (- font-height font-descent))
  (define to-next-y (+ font-height 1))
  ;; unaligned and center elements must consider the lowest point on the line
  ;; some text on the line could be in a different font and have a lower descent
  ;; from the baseline
  (define (calc-next-line-y y ll)
    (if (empty? ll)
        (+ y to-next-y)
        (max (+ y to-next-y)
             (+ (bottom-edge-of-line ctx ll) 1))))
  
  (case (element-alignment e)
    [(right)
     (let loop ([words (element-words e)]
                [lines '()]
                [line-start-pos 0]
                [ypos y]
                [x (- total-width (layout-context-right-width ctx))]
                [max-width 0])
       ;(printf "loop right: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
       (define-values (last-word width remaining-words)
         (layout-remainder-of-line words (- total-width (layout-context-left-width ctx) (unaligned-or-center-width ctx) (layout-context-right-width ctx))))
       ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
       (cond
         [(and (false? last-word) (empty? remaining-words))
          ; final iteration
          ;(printf " final iteration~n")
          ;; add last line to layout element list
          (set-layout-context-right-elements! ctx (cons (car lines) (layout-context-right-elements ctx)))
          (set-layout-context-right-width! ctx (+ (layout-context-right-width ctx) (wrapped-line-w (car lines))))
          ;(printf " end line width=~a, lrw=~a~n" (wrapped-line-w (car lines)) (layout-context-right-width ctx))
          ;; set the element's lines field and put the lines in order
          (set-element-lines! e (reverse lines))
          (values x y (+ x max-width) (+ ypos font-height))]
         [(and (false? last-word) (empty? (unaligned-or-center-elements ctx)) (empty? (layout-context-left-elements ctx)) (empty? (layout-context-right-elements ctx)))
          ; first word is too long to fit on a line, just place it at the beginning of line
          (printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
          (define xpos 0)
          (define new-ypos ypos)
          (when (not (empty? (cdr remaining-words)))
            (set! new-ypos (+ ypos to-next-y))
            (layout-goto-new-line ctx new-ypos))
          (loop (cdr remaining-words)
                (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                (word-end-pos (car remaining-words))
                new-ypos
                (min xpos x)
                (max width max-width))]
         [(false? last-word)
          ; first word is too long, advance line and try again
          (define new-ypos (+ ypos to-next-y))
          (layout-goto-new-line ctx new-ypos)
          (loop remaining-words
                lines
                line-start-pos
                new-ypos
                x
                max-width)]
         [else
          (define xpos (- total-width (layout-context-right-width ctx) width))
          (define new-ypos ypos)
          (when (not (empty? remaining-words))
            (set! new-ypos (+ ypos to-next-y))
            (layout-goto-new-line ctx new-ypos))
          (loop remaining-words
                (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                (word-end-pos last-word)
                new-ypos
                (min xpos x)
                (max width max-width))]))]
    [(center)
     (define baseline (+ y height))
     (when (> baseline (layout-context-baseline-pos ctx))
       (when (not (empty? (layout-context-center-elements ctx)))
         (define diff (- baseline (layout-context-baseline-pos ctx)))
         (adjust-elements-ypos! (layout-context-center-elements ctx) diff))
       (set-layout-context-baseline-pos! ctx baseline))
     
     (let loop ([words (element-words e)]
                [lines '()]
                [line-start-pos 0]
                [ypos (- (layout-context-baseline-pos ctx) height)]
                [x total-width]
                [max-width 0])
       ;(printf "loop center: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
       (define-values (last-word width remaining-words)
         (layout-remainder-of-line words (- total-width (layout-context-left-width ctx) (layout-context-center-width ctx) (layout-context-right-width ctx))))
       ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
       (cond
         [(and (false? last-word) (empty? remaining-words))
          ; final iteration
          ;(printf " final iteration~n")
          ;; update the baseline position after all lines are placed
          (set-layout-context-baseline-pos! ctx (+ ypos height))
          ;; add last line to layout element list
          (set-layout-context-center-elements! ctx (cons (car lines) (layout-context-center-elements ctx)))
          (set-layout-context-center-width! ctx (+ (layout-context-center-width ctx) (wrapped-line-w (car lines))))
          ;(printf " end line width=~a, lcw=~a~n" (wrapped-line-w (car lines)) (layout-context-center-width ctx))
          ;; set the element's lines field and put the lines in order
          (set-element-lines! e (reverse lines))
          (values x y (+ x max-width) (+ ypos font-height))]
         [(and (not (empty? (layout-context-center-elements ctx))) (false? last-word))
          ; there is a partial line of centered elements
          ; first word is too long, advance line and try again
          (define new-ypos (calc-next-line-y ypos (layout-context-center-elements ctx)))
          (layout-goto-new-line ctx new-ypos)
          (loop remaining-words
                lines
                line-start-pos
                new-ypos
                x
                max-width)]
         [(not (empty? (layout-context-center-elements ctx)))
          ; there is a partial line of centered elements, shift existing elements over to make room
          (define old-margin ( / (- total-width (layout-context-left-width ctx) (layout-context-center-width ctx) (layout-context-right-width ctx)) 2))
          (define margin ( / (- total-width (layout-context-left-width ctx) (layout-context-center-width ctx) (layout-context-right-width ctx) width) 2))
          (define diff (- margin old-margin))
          ;(printf "shift centered elements over: lcw=~a, width =~a, margin ~a to ~a, diff=~a~n" (layout-context-center-width ctx) width old-margin margin diff)
          (adjust-elements-xpos! (layout-context-center-elements ctx) diff)
          ;;
          (define xpos (+ (layout-context-left-width ctx) margin (layout-context-center-width ctx)))
          (define new-ypos ypos)
          (when (not (empty? remaining-words))
            (set! new-ypos (calc-next-line-y ypos (layout-context-center-elements ctx)))
            (layout-goto-new-line ctx new-ypos))
          (loop remaining-words
                (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                (word-end-pos last-word)
                new-ypos
                (min xpos x)
                (max width max-width))]
         [(and (false? last-word) (empty? (layout-context-center-elements ctx)) (empty? (layout-context-left-elements ctx)) (empty? (layout-context-right-elements ctx)))
          ; first word is too long to fit on a line, just place it at the beginning of line
          ;(printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
          (define xpos 0)
          (define new-ypos ypos)
          (when (not (empty? (cdr remaining-words)))
            (set! new-ypos (calc-next-line-y ypos (layout-context-center-elements ctx)))
            (layout-goto-new-line ctx new-ypos))
          (loop (cdr remaining-words)
                (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                (word-end-pos (car remaining-words))
                new-ypos
                (min xpos x)
                (max width max-width))]
         [(false? last-word)
          ; first word is too long, advance line and try again
          (define new-ypos (calc-next-line-y ypos (layout-context-center-elements ctx)))
          (layout-goto-new-line ctx new-ypos)
          (loop remaining-words
                lines
                line-start-pos
                new-ypos
                x
                max-width)]
         [else
          (define space-available (- total-width (layout-context-left-width ctx) (layout-context-right-width ctx)))
          (define space-leftover (- space-available width))
          (define xpos (+ (layout-context-left-width ctx) (/ space-leftover 2)))
          (define new-ypos ypos)
          (when (not (empty? remaining-words))
            (set! new-ypos (calc-next-line-y ypos (layout-context-center-elements ctx)))
            (layout-goto-new-line ctx new-ypos))
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
                [x (layout-context-left-width ctx)]
                [max-width 0])
       ;(printf "loop left: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
       (define-values (last-word width remaining-words)
         (layout-remainder-of-line words (- total-width (layout-context-left-width ctx) (unaligned-or-center-width ctx) (layout-context-right-width ctx))))
       ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
       (cond
         [(and (false? last-word) (empty? remaining-words))
          ; final iteration
          ;(printf " final iteration~n")
          ;; add last line to layout element list
          (set-layout-context-left-elements! ctx (cons (car lines) (layout-context-left-elements ctx)))
          (set-layout-context-left-width! ctx (+ (layout-context-left-width ctx) (wrapped-line-w (car lines))))
          ;(printf " end line width=~a, llw=~a~n" (wrapped-line-w (car lines)) (layout-context-left-width ctx))
          ;; set the element's lines field and put the lines in order
          (set-element-lines! e (reverse lines))
          (values x y (+ x max-width) (+ ypos font-height))]
         [(and (false? last-word) (empty? (unaligned-or-center-elements ctx)) (empty? (layout-context-left-elements ctx)) (empty? (layout-context-right-elements ctx)))
          ; first word is too long to fit on a line, just place it at the beginning of line
          ;(printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
          (define xpos 0)
          (define new-ypos ypos)
          (when (not (empty? (cdr remaining-words)))
            (set! new-ypos (+ ypos to-next-y))
            (layout-goto-new-line ctx new-ypos))
          (loop (cdr remaining-words)
                (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                (word-end-pos (car remaining-words))
                new-ypos
                (min xpos x)
                (max width max-width))]
         [(false? last-word)
          ; first word is too long, advance line and try again
          (define new-ypos (+ ypos to-next-y))
          (layout-goto-new-line ctx new-ypos)
          (loop remaining-words
                lines
                line-start-pos
                new-ypos
                x
                max-width)]
         [else
          (define xpos (layout-context-left-width ctx))
          (define new-ypos ypos)
          (when (not (empty? remaining-words))
            (set! new-ypos (+ ypos to-next-y))
            (layout-goto-new-line ctx new-ypos))
          (loop remaining-words
                (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                (word-end-pos last-word)
                new-ypos
                (min xpos x)
                (max width max-width))]))]
    [(unaligned)
     (define baseline (+ y height))
     ;(printf "unaligned baseline=~a/~a, y=~a, height=~a, font-height=~a~n" (layout-context-baseline-pos ctx) baseline y height font-height) 
     (when (> baseline (layout-context-baseline-pos ctx))
       (when (not (empty? (layout-context-unaligned-elements ctx)))
         (define diff (- baseline (layout-context-baseline-pos ctx)))
         (adjust-elements-ypos! (layout-context-unaligned-elements ctx) diff))
       (set-layout-context-baseline-pos! ctx baseline))

     (let loop ([words (element-words e)]
                [lines '()]
                [line-start-pos 0]
                [ypos (- (layout-context-baseline-pos ctx) height)]
                [x (+ (layout-context-left-width ctx) (layout-context-unaligned-width ctx))]
                [max-width 0])
       ;(printf "loop unaligned: start=~a, ypos=~a, x=~a, max-width=~a~n" line-start-pos ypos x max-width)
       (define-values (last-word width remaining-words)
         (layout-remainder-of-line words (- total-width (layout-context-left-width ctx) (layout-context-unaligned-width ctx) (layout-context-right-width ctx))))
       ;(printf " last-word=~a, width=~a, remaining=~a~n" last-word width remaining-words)
       (cond
         [(and (false? last-word) (empty? remaining-words))
          ; final iteration
          ;; update the baseline position after all lines are placed
          ;(printf " final iteration~n")
          (set-layout-context-baseline-pos! ctx (+ ypos height))
          ;; add last line to layout element list
          (set-layout-context-unaligned-elements! ctx (cons (car lines) (layout-context-unaligned-elements ctx)))
          (set-layout-context-unaligned-width! ctx (+ (layout-context-unaligned-width ctx) (wrapped-line-w (car lines))))
          ;(printf " end line width=~a, luw=~a~n" (wrapped-line-w (car lines)) (layout-context-unaligned-width ctx))
          ;; set the element's lines field and put the lines in order
          (set-element-lines! e (reverse lines))
          (values x y (+ x max-width) (+ ypos font-height))]
         [(and (false? last-word) (empty? (layout-context-unaligned-elements ctx)) (empty? (layout-context-left-elements ctx)) (empty? (layout-context-right-elements ctx)))
          ; first word is too long to fit on a line, just place it at the beginning of line
          ;(printf " unable to wrap word ~a, place it anyway~n" (word-str (car remaining-words)))
          (define xpos 0)
          (define new-ypos ypos)
          (when (not (empty? (cdr remaining-words)))
            (set! new-ypos (calc-next-line-y ypos (layout-context-unaligned-elements ctx)))
            (layout-goto-new-line ctx new-ypos))
          (loop (cdr remaining-words)
                (cons (wrapped-line line-start-pos (word-end-pos (car remaining-words)) xpos ypos (word-width (car remaining-words)) font-height) lines)
                (word-end-pos (car remaining-words))
                new-ypos
                (min xpos x)
                (max width max-width))]
         [(false? last-word)
          ; first word is too long, advance line and try again
          (define new-ypos (calc-next-line-y ypos (layout-context-unaligned-elements ctx)))
          (layout-goto-new-line ctx new-ypos)
          (loop remaining-words
                lines
                line-start-pos
                new-ypos
                x
                max-width)]
         [else
          (define xpos (+ (layout-context-left-width ctx) (layout-context-unaligned-width ctx)))
          (define new-ypos ypos)
          (when (not (empty? remaining-words))
            (set! new-ypos (calc-next-line-y ypos (layout-context-unaligned-elements ctx)))
            (layout-goto-new-line ctx new-ypos))
          (loop remaining-words
                (cons (wrapped-line line-start-pos (word-end-pos last-word) xpos ypos width font-height) lines)
                (word-end-pos last-word)
                new-ypos
                (min xpos x)
                (max width max-width))]))]))

(define (layout-snip ctx e total-width y ew eh [valign 'bottom])
  (define (initial-baseline top height valign)
    (case valign
      [(top)
       (+ top height)]
      [(middle)
       (+ top (ceiling (/ height 2)))]
      [else
       ; bottom
       (+ top height)]))
  
  (define (adjust-for-valign top bottom valign elist)
    (case valign
      [(top)
       (when (> bottom (layout-context-baseline-pos ctx))
         ; push the baseline down to meet bottom of snip
         (define diff (- bottom (layout-context-baseline-pos ctx)))
         ;(printf "adjust-for-valign: top adjust baseline by ~a~n" diff)
         (adjust-elements-ypos! elist diff)
         (set-layout-context-baseline-pos! ctx bottom))
       ;(printf "adjust-for-valign: top ~a-~a~n" top bottom)
       (values top bottom)]
      [(middle)
       (define height (- bottom top))
       (define new-top (max 0 (- (layout-context-baseline-pos ctx) (/ height 2))))
       (define new-bottom (+ new-top height))
       (cond
         [(< new-top top)
          ; push the baseline down
          (define diff (- top new-top))
          (adjust-elements-ypos! elist diff)
          (set-layout-context-baseline-pos! ctx (+ (layout-context-baseline-pos ctx) diff))
          ;(printf "adjust-for-valign: middle ~a,~a,~a,~a~n" height new-top new-bottom diff)
          ;(printf "adjust-for-valign: middle 1 ~a-~a->~a-~a~n" top bottom top (+ bottom diff))
          (values top (+ bottom diff))]
         [(>= new-top top)
          ;(printf "adjust-for-valign: middle 2 ~a-~a->~a-~a~n" top bottom new-top new-bottom)
          (values new-top new-bottom)])]
      [else
       ; bottom
       ;(printf "adjust-for-valign: bottom=~a baseline=~a~n" bottom (layout-context-baseline-pos ctx))
       (cond
         [(> bottom (layout-context-baseline-pos ctx))
          ; push the baseline down to meet bottom of snip
          (define diff (- bottom (layout-context-baseline-pos ctx)))
          (adjust-elements-ypos! elist diff)
          (set-layout-context-baseline-pos! ctx bottom)
          ;(printf "adjust-for-valign: bottom adjust baseline by ~a~n" diff)
          ;(printf "adjust-for-valign: bottom 1 ~a-~a~n" top bottom)
          (values top bottom)]
         [(< bottom (layout-context-baseline-pos ctx))
          ; adjust y position to touch the baseline
          (define diff (- (layout-context-baseline-pos ctx) bottom))
          ;(printf "adjust-for-valign: bottom 2 ~a-~a->~a-~a~n" top bottom (+ top diff) (+ bottom diff))
          (values (+ top diff) (+ bottom diff))]
         [else
          (values top bottom)])]))
  
  (if (< (- total-width (+ (layout-context-left-width ctx) (layout-context-right-width ctx) (unaligned-or-center-width ctx))) ew)
      ; we don't have room for this element on the current line/y-position
      (let ([new-y (next-line-y-pos ctx y)])
        (if (not (= y new-y))
            (begin
              (layout-goto-new-line ctx new-y)
              (layout-snip ctx e total-width new-y ew eh valign))
            ; we have advanced the current y position as far as we can and it still doesn't fit
            (begin
              (case (element-alignment e)
                [(left)
                 (set-layout-context-left-elements! ctx (cons e (layout-context-left-elements ctx)))
                 (set-layout-context-left-width! ctx (+ ew (layout-context-snip-xmargin ctx)))]
                [(right)
                 (set-layout-context-right-elements! ctx (cons e (layout-context-right-elements ctx)))
                 (set-layout-context-right-width! ctx (+ ew (layout-context-snip-xmargin ctx)))]
                [else
                 (set-layout-context-unaligned-elements! ctx (cons e (layout-context-unaligned-elements ctx)))
                 (set-layout-context-unaligned-width! ctx (+ ew (layout-context-snip-xmargin ctx)))])
              (values 0 y ew (+ y eh)))))
      ; we do have room
      (case (element-alignment e)
        [(left)
         ;(printf "layout left aligned element~n")
         (if (empty? (layout-context-left-elements ctx))
             (begin
               (set-layout-context-left-elements! ctx (cons e (layout-context-left-elements ctx)))
               (set-layout-context-left-width! ctx (+ ew (layout-context-snip-xmargin ctx)))
               ; shift unaligned elements over since we inserted this element to the left
               (adjust-elements-xpos! (layout-context-unaligned-elements ctx) (+ ew (layout-context-snip-xmargin ctx)))
               (values 0 y ew (+ y eh)))
             (let ([x1 (layout-context-left-width ctx)]
                   [y1 y]
                   [x2 (+ (layout-context-left-width ctx) ew)]
                   [y2 (+ y eh)])
               (set-layout-context-left-elements! ctx (cons e (layout-context-left-elements ctx)))
               (set-layout-context-left-width! ctx (+ (layout-context-left-width ctx) ew (layout-context-snip-xmargin ctx)))
               ; shift unaligned elements over since we inserted this element to the left
               (adjust-elements-xpos! (layout-context-unaligned-elements ctx) (+ ew (layout-context-snip-xmargin ctx)))
               (values x1 y1 x2 y2)))]
        [(right)
         ;(printf "layout right aligned element~n")
         (if (empty? (layout-context-right-elements ctx))
             (begin
               (set-layout-context-right-elements! ctx (cons e (layout-context-right-elements ctx)))
               (set-layout-context-right-width! ctx (+ ew (layout-context-snip-xmargin ctx)))
               (values (- total-width ew) y total-width (+ y eh)))
             (let ([x1 (- total-width (layout-context-right-width ctx) ew)]
                   [y1 y]
                   [x2 (- total-width (layout-context-right-width ctx))]
                   [y2 (+ y eh)])
               (set-layout-context-right-elements! ctx (cons e (layout-context-right-elements ctx)))
               (set-layout-context-right-width! ctx (+ (layout-context-right-width ctx) ew (layout-context-snip-xmargin ctx)))
               (values x1 y1 x2 y2)))]
        [(center)
         ;(printf "layout center aligned element~n")
         ; we assume that center and unaligned elements are mutually exclusive on the same line
         ; the canvas user must end a line before adding an element with the other alignment
         ; otherwise, behavior is undefined
         (if (empty? (layout-context-center-elements ctx))
             (let ([margin (/ (- total-width (layout-context-left-width ctx) (layout-context-right-width ctx) ew) 2)])
               (set-layout-context-center-elements! ctx (cons e (layout-context-center-elements ctx)))
               (set-layout-context-center-width! ctx (+ ew (layout-context-snip-xmargin ctx)))
               (set-layout-context-baseline-pos! ctx (initial-baseline y eh valign))
               (values (+ (layout-context-left-width ctx) margin) y (+ (layout-context-left-width ctx) margin ew) (+ y eh)))
             (let* ([margin (/ (- total-width (layout-context-left-width ctx) (layout-context-right-width ctx) (layout-context-center-width ctx) ew) 2)]
                    [pos (+ (layout-context-left-width ctx) margin (layout-context-center-width ctx))]
                    [y1 y]
                    [y2 (+ y eh)])
               ; reposition each centered element on the line
               (define old-margin ( / (- total-width (layout-context-left-width ctx) (layout-context-center-width ctx) (layout-context-right-width ctx)) 2))
               (define diff (- margin old-margin))
               (adjust-elements-xpos! (layout-context-center-elements ctx) diff)
               (set!-values (y1 y2) (adjust-for-valign y1 y2 valign (layout-context-center-elements ctx)))
               (set-layout-context-center-elements! ctx (cons e (layout-context-center-elements ctx)))
               (set-layout-context-center-width! ctx (+ (layout-context-center-width ctx) ew (layout-context-snip-xmargin ctx)))
               (values pos y1 (+ pos ew) y2)))]
        [(unaligned)
         ;(printf "layout unaligned element~n")
         (if (empty? (layout-context-unaligned-elements ctx))
             (begin
               (set-layout-context-unaligned-elements! ctx (cons e (layout-context-unaligned-elements ctx)))
               (set-layout-context-unaligned-width! ctx (+ ew (layout-context-snip-xmargin ctx)))
               (set-layout-context-baseline-pos! ctx (initial-baseline y eh valign))
               (values (layout-context-left-width ctx) y (+ (layout-context-left-width ctx) ew) (+ y eh)))
             (let ([x1 (+ (layout-context-left-width ctx) (layout-context-unaligned-width ctx))]
                   [y1 y]
                   [x2 (+ (layout-context-left-width ctx) (layout-context-unaligned-width ctx) ew)]
                   [y2 (+ y eh)])
               (set!-values (y1 y2) (adjust-for-valign y1 y2 valign (layout-context-unaligned-elements ctx)))
               (set-layout-context-unaligned-elements! ctx (cons e (layout-context-unaligned-elements ctx)))
               (set-layout-context-unaligned-width! ctx (+ (layout-context-unaligned-width ctx) ew (layout-context-snip-xmargin ctx)))
               (values x1 y1 x2 y2)))]
        [else
         (error "invalid alignment!")])))
