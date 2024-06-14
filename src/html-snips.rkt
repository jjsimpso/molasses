#lang racket/gui

(require racket/class
         racket/snip
         racket/draw
         racket/format
         net/sendurl)

(require "request.rkt")

(provide horz-line-snip%
         ul-bullet-snip%
         img-hack-snip%
         html-image-snip%
         html-map%
         html-map-img-snip%
         html-link-snip%
         html-link-img-snip%)

(define (next-break-pos s pos)
  (define (line-break? c)
    (char=? c #\space))
  
  (for/last ([c (in-string s pos)]
             [i (in-naturals pos)]
             #:final (line-break? c))
    ;(printf "c[~a]=~a~n" i c)
    (if (line-break? c)
        i
        #f)))

(define img-hack-snip%
  (class snip%
    (super-new)
    (init-field [in #f]
                [align 'left]
                [horz-offset 5])
    
    (define img (if in
                    (make-object bitmap% in)
                    ; fill in later with default image
                    (make-object bitmap% 64 64)))

    ;; list of strings to draw in order with their style% and alignment
    (define str-list '())
    (define (str-string str)
      (first str))
    (define (str-style str)
      (second str))
    (define (str-alignment str)
      (third str))

    (define/public (push s style alignment)
      (set! str-list (append str-list (list (list s style alignment)))))
    
    (define (calc-width dc-width)
      (- dc-width (* horz-offset 2)))
    
    (define/override (get-extent dc x y	 	 	 	 
                                   [w #f]
                                   [h #f]
                                   [descent #f]
                                   [space #f]
                                   [lspace #f]
                                   [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (define-values (width height) (send dc get-size))
      
      (maybe-set-box! w (calc-width width))
      (maybe-set-box! h (send img get-height))
      (maybe-set-box! descent 0.0)
      (maybe-set-box! space 0.0)
      (maybe-set-box! lspace 0.0)
      (maybe-set-box! rspace 0.0))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define-values (w h) (send dc get-size))
      (define img-width (send img get-width))
      ;; text origin
      (define-values (tx ty) (values x y))
      (define endx w)
      
      ;; draw image
      (cond
        [(and (eq? align 'right)
              (< img-width w))
         ;(eprintf "img draw: x=~a, w=~a, img-width=~a~n" x w img-width)
         (set! endx (- w img-width)) ; need to add some margin to this
         (send dc draw-bitmap img (- w img-width) y)]
        [else
         (set! tx (+ x img-width)) ; need to add some margin to this
         (send dc draw-bitmap img x y)])
      ;; draw text
      (define curx tx)
      (define cury ty)
      (for ([str (in-list str-list)])
        (define s (string-trim (str-string str) #:left? #f))
        (define style (str-style str))
        (let loop ([prev-end 0]
                   [start 0]
                   [end (next-break-pos s 0)])
          (define-values (line-width line-height descent extra)
            (if (not end)
                (send dc get-text-extent (substring s start))
                (send dc get-text-extent (substring s start end))))
          ;(eprintf "loop end=~a, curx=~a, endx=~a, linew=~a~n" end curx endx line-width)
          (cond
            [(> (+ curx line-width) endx)
             ;; draw a line of text and move to the next line
             ;(eprintf "drawing text ~a~n" (substring s start prev-end))
             (send dc draw-text (substring s start prev-end) curx cury #f)
             (set! curx tx)
             (set! cury (+ cury line-height))
             (loop (add1 prev-end) (add1 prev-end) end)]
            [(not end)
             ;; end of string but we don't have a full line yet
             ;; write the partial line and increment starting x coordinate
             ;(eprintf "drawing partial text ~a~n" (substring s start))
             (send dc draw-text (substring s start) curx cury #f)
             ; don't worry about space at end for now
             (set! curx (+ curx line-width))]
            [else
             ;; add another word to the line
             (loop end start (next-break-pos s (add1 end)))]))))

    ))

(define horz-line-snip%
  (class snip%
    (super-new)

    (init-field
     ; the left margin/horizontal inset of the editor-canvas<%>
     [horz-offset 5]
     [w 100]
     [h 8])

    (define width w)
    (define height h)
    
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
      (printf "resizing horiz-line-snip% to ~ax~a~n" w h)
      (set! width w)
      (set! height h))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      
      ;; draw debug rectangle
      #|
      (define old-brush (send dc get-brush))
      (send dc set-pen "blue" 1 'transparent)
      (send dc set-brush "green" 'solid)
      (send dc draw-rectangle x y (+ x drawable-width) height)
      ;(send dc draw-line x y (+ x drawable-width) y)
      (send dc set-brush old-brush)
      |#
      (send dc set-pen (make-object color% #x9a #x9a #x9a) 1 'solid)
      (send dc set-smoothing 'aligned)

      (define x-pos x)
      (define y-pos (sub1 (+ y (/ height 2))))
      (define y-pos-line2 (add1 y-pos))
      ;(eprintf "draw: width=~a draw ~a to ~a~n" width x-pos (+ x-pos width))
      (send dc draw-line x-pos                  y-pos
                         (sub1 (+ x-pos width)) y-pos)
      (send dc set-pen (make-object color% #xba #xba #xba) 1 'solid)
      (send dc draw-point (+ x-pos width) y-pos)
      (send dc set-pen (make-object color% #xb9 #xb9 #xb9) 1 'solid)
      (send dc draw-point x-pos y-pos-line2)
      (send dc set-pen (make-object color% #xee #xee #xee) 1 'solid)
      (send dc draw-line (add1 x-pos)      y-pos-line2
                         (+ x-pos width) y-pos-line2)

      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen))

    (define/override (copy)
      (new horz-line-snip% [horz-offset horz-offset]))))

(define ul-bullet-snip%
  (class snip%
    (super-new)

    (init-field
     [style 'disc]
     [size 1.0])


    (define width (* size 10))
    (define height (* size 10))
    (define xoffset (/ width 5))
    (define yoffset (/ height 2))
    (define diameter (- width yoffset))
    
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

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      (define old-brush (send dc get-brush))
      
      (send dc set-smoothing 'aligned)

      (case style
        [(disc)
         (send dc set-pen "black" 1 'transparent)
         (send dc set-brush "black" 'solid)
         (send dc draw-ellipse (+ x xoffset) (+ y yoffset) diameter diameter)]
        [(circle)
         (send dc set-pen "black" 1 'solid)
         (send dc set-brush "black" 'transparent)
         (send dc draw-ellipse (+ x xoffset) (+ y yoffset) diameter diameter)]
        [(square)
         (send dc set-pen "black" 1 'transparent)
         (send dc set-brush "black" 'solid)
         (send dc draw-rectangle (+ x xoffset) (+ y yoffset) diameter diameter)])
      
      (send dc set-pen old-pen)
      (send dc set-smoothing old-smoothing)
      (send dc set-brush old-brush))

    (define/override (copy)
      (new ul-bullet-snip% [style style] [size size]))))

(define html-image-snip%
  (class image-snip%
    (field
     [base-bitmap #f]
     [base-width 0]
     [base-height 0])

    (super-new)

    (init-field
     [hspace 2]
     [vspace 0])

    (define/override (set-bitmap bm [mask #f])
      (when bm
        (set! base-bitmap bm)
        (set! base-width (send bm get-width))
        (set! base-height (send bm get-height)))
      (super set-bitmap bm mask))

    (define/override (resize w h)
      (define width (exact-round w))
      (define height (exact-round h))
      (define new-bm (make-bitmap width height))
      (define new-bm-dc (new bitmap-dc% (bitmap new-bm)))
      (send new-bm-dc set-scale (/ width base-width) (/ height base-height))
      (send new-bm-dc draw-bitmap base-bitmap 0 0)
      (set-bitmap new-bm))
    
    (define/override (get-extent dc x y
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (super get-extent dc (or x 0) (or y 0) w h descent space lspace rspace)
      (when w (set-box! w (+ (unbox w) (* 2 hspace))))
      (when h (set-box! h (+ (unbox h) (* 2 vspace)))))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (super draw dc (+ x hspace) (+ y vspace) left top right bottom dx dy draw-caret))))

(define html-map%
  (class object%
    (super-new)

    (struct point (x y) #:transparent)
    
    (struct area
      (shape
       points
       url
       [radius #:mutable #:auto])
      #:transparent #:auto-value #f)
    
    (define areas '())

    (define/public (add-rect left top right bottom url)
      ;(printf "adding rect with url ~a~n" url)
      (set! areas (cons (area 'rect (cons (point left top) (point right bottom)) url) areas)))

    ;; coords is a vector of coordinates alternating x and y
    (define/public (add-poly coords url)
      (define points
        (for/vector ([i (in-range 0 (vector-length coords) 2)])
          (point (vector-ref coords i) (vector-ref coords (add1 i)))))
      ;(printf "add-poly ~a~n" points)
      (set! areas (cons (area 'poly points url) areas)))

    (define/public (add-circle x y radius url)
      (define a (area 'circle (list (point x y)) url))
      (set-area-radius! a radius)
      (set! areas (cons a areas)))

    (define (point-in-rect? a x y)
      (define rect-points (area-points a))
      (and (>= x (point-x (car rect-points)))
           (<= x (point-x (cdr rect-points)))
           (>= y (point-y (car rect-points)))
           (<= y (point-y (cdr rect-points)))))

    (define (point-in-poly? a x y)
      (define (cross-product u v)
        (- (* (point-x u) (point-y v))
           (* (point-y u) (point-x v))))
      (define poly-points (area-points a))
      (define num-points (vector-length poly-points))
      ;(printf "point-in-poly? num-points=~a~n" num-points)
      ;; stop when a point is outside of the polygon and return #f, otherwise #t
      (for/and ([o (in-vector poly-points)]
                [i (in-naturals 1)])
        (define u
          (if (< i num-points)
              (point (- (point-x (vector-ref poly-points i)) (point-x o))
                     (- (point-y (vector-ref poly-points i)) (point-y o)))
              (point (- (point-x (vector-ref poly-points 0)) (point-x o))
                     (- (point-y (vector-ref poly-points 0)) (point-y o)))))
        (define v (point (- x (point-x o)) (- y (point-y o))))
        ;(printf "  ~a x ~v = ~a~n" u v (cross-product u v))
        ;; test is for points in counter clockwise order (?)
        (>= (cross-product u v) 0)))
  
    (define (point-in-circle? a x y)
      (define c (car (area-points a)))
      (define dist (sqrt (+ (sqr (- x (point-x c)))
                            (sqr (- y (point-y c))))))
      (<= dist (area-radius a)))
    
    (define (point-in-area? a x y)
      (case (area-shape a)
        [(rect)
         (point-in-rect? a x y)]
        [(poly)
         (point-in-poly? a x y)]
        [(circle)
         (point-in-circle? a x y)]
        [else
         #f]))

    ;; return the area's url if (x,y) is within an area, otherwise #f
    (define/public (inside? x y)
      ;(printf "inside ~a,~a~n" x y)
      (for/or ([a (in-list areas)])
        ;(printf "  checking area: ~a~n" a)
        (if (point-in-area? a x y)
            (area-url a)
            #f)))

    (define/public (print-areas)
      (for ([a (in-list areas)])
        (printf "~a~n" a)))))


(define (follow-link browser-canvas base-req url)
  (define (replace-final-path-element path relative-path)
    ;(printf "replace-final-path-element: ~a, ~a~n" path relative-path)
    (string-append (path->string (path-only path)) relative-path))

  (define (guess-type-from-filename path)
    (define extension (path-get-extension path))
    (if extension
        (cond
          [(or (bytes=? extension #".htm")
               (bytes=? extension #".html"))
           #\h]
          [(or (bytes=? extension #".txt")
               (bytes=? extension #".conf")
               (bytes=? extension #".cfg")
               (bytes=? extension #".sh")
               (bytes=? extension #".bat")
               (bytes=? extension #".ini"))
           #\0]
          [(bytes=? extension #".gif") "g"]
          [(or (bytes=? extension #".jpg")
               (bytes=? extension #".jpeg")
               (bytes=? extension #".bmp")
               (bytes=? extension #".xpm")
               (bytes=? extension #".ppm")
               (bytes=? extension #".tiff")
               (bytes=? extension #".png"))
           #\I]
          [(or (bytes=? extension #".wav")
               (bytes=? extension #".ogg")
               (bytes=? extension #".mp3"))
           #\s]
          [else
           #\9])
        #\h))

  (define (request-from-partial-url url)
    (struct-copy request
                 base-req
                 [type (guess-type-from-filename url)]
                 [path/selector
                  (if (equal? (string-ref url 0) #\/)
                      url
                      (replace-final-path-element (request-path/selector base-req) url))]))
  
  (eprintf "following html link: ~a, base-req path=~a~n" url (request-path/selector base-req))
  (cond
    [(string-contains? url "#")
     (define base-file (last (string-split (request-path/selector base-req) "/")))
     (define link-file (string-trim url #px"#.+" #:left? #f))
     (define anchor (string-trim url #px".*#" #:right? #f))
     (if (or (equal? (string-ref url 0) #\#)
             (equal? base-file link-file))
         ;; jump to anchor location on current page
         (let ([y (send browser-canvas find-anchor-position anchor)])
           (and y (send browser-canvas scroll-to y)))
         (send browser-canvas go
               (struct-copy request
                            base-req
                            [type #\h]
                            [path/selector
                             (if (equal? (string-ref link-file 0) #\/)
                                 link-file
                                 (replace-final-path-element (request-path/selector base-req) link-file))])
               anchor))]
    [(regexp-match #px"^(\\w+://).*" url)
     (cond
       [(string-prefix? url "gemini://")
        (send browser-canvas go (url->request url))]
       [(string-prefix? url "gopher://")
        (send browser-canvas go (url->request url))]
       [(or (string-prefix? url "http://")
            (string-prefix? url "https://"))
        (send-url url #t)])]
    [else
     ;; handle partial URLs
     (send browser-canvas go (request-from-partial-url url))]))

(define html-map-img-snip%
  (class html-image-snip%
    (init-field browser-canvas
                img-map
                base-req)

    (define use-hand-cursor #f)
    
    (super-new)

    (define (set-hand-cursor)
      (set! use-hand-cursor #t)
      (send browser-canvas set-cursor (make-object cursor% 'hand)))

    (define (unset-hand-cursor)
      (set! use-hand-cursor #f)
      (send browser-canvas set-cursor #f)
      (send browser-canvas update-status "Ready"))
    
    (define/override (adjust-cursor dc x y editorx editory event)
      (when (send event entering?)
        (eprintf "adjust-cursor: mouse entering~n")))
      
    (define/override (on-event dc x y editorx editory event)
      ;(eprintf "html-map-img-snip% mouse event ~a at ~a,~a~n" (send event get-event-type) (send event get-x) (send event get-y))
      (define-values (imgx imgy) (values (- (send event get-x) x)
                                         (- (send event get-y) y)))
 
      (cond
        [(send event moving?)
         ;(eprintf "mouse motion event~n")
         (define area-url (send img-map inside? imgx imgy))
         (cond
           [(and area-url (not use-hand-cursor))
            (set-hand-cursor)
            (send browser-canvas update-status (string-append "Goto " area-url))]
           [(and use-hand-cursor (not area-url))
            (on-goodbye-event dc x y editorx editory event)]
           [else
            void])]
        [(send event button-down? 'left)
         (define area-url (send img-map inside? imgx imgy))
         (when area-url
           (printf "base-req=~a url=~a~n" base-req area-url)
           (on-goodbye-event dc x y editorx editory event)
           (follow-link browser-canvas base-req area-url))]))

    (define/override (on-goodbye-event dc x y editorx editory event)
      (eprintf "goodbye event~n")
      (send browser-canvas update-status "Ready")
      (unset-hand-cursor))
    
    ))

(define link-interface (interface () on-event on-goodbye-event get-flags set-flags))

(define (html-link-mixin %)
  (class %
    (init-field [url ""]
                [base-req #f]
                [browser-canvas #f])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    (set-flags (cons 'handles-between-events (get-flags)))

    (define status-text (string-append "Goto " url))

    (define/override (adjust-cursor dc x y editorx editory event)
      (when (send event entering?)
        (eprintf "adjust-cursor: mouse entering~n")
        (send browser-canvas set-cursor (make-object cursor% 'hand))))
      
    (define/override (on-event dc x y editorx editory event)
      ;(eprintf "html-link-snip% mouse event ~a~n" (send event get-event-type))
      (cond
        [(send event moving?)
         ;(eprintf "mouse motion event~n")
         (send browser-canvas update-status status-text)]
        [(send event button-down? 'left)
         (on-goodbye-event dc x y editorx editory event)
         (follow-link browser-canvas base-req url)]))

    (define/override (on-goodbye-event dc x y editorx editory event)
      (eprintf "goodbye event~n")
      (send browser-canvas set-cursor #f)
      (send browser-canvas update-status "Ready"))))

(define html-link-snip% (html-link-mixin string-snip%))
(define html-link-img-snip% (html-link-mixin html-image-snip%))

#;(module+ test (require rackunit)
  (define map (new html-map%))  
)
  
