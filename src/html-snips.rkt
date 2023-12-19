#lang racket

(require racket/class
         racket/snip
         racket/draw
         racket/format
         net/sendurl)

(require "request.rkt")

(provide horz-line-snip%
         img-hack-snip%
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

(define link-interface (interface () on-event on-goodbye-event get-flags set-flags))

(define (html-link-mixin %)
  (class %
    (init-field [url ""]
                [base-url ""]
                [browser-canvas #f])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    (set-flags (cons 'handles-between-events (get-flags)))

    (define status-text (string-append "Goto " url))

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

    (define (follow-link)
      (eprintf "following html link: ~a~n" url)
      (if (regexp-match #px"^(\\w+://).*" url)
          (cond
            [(string-prefix? url "gemini://")
             (send browser-canvas go (url->request url))]
            [(string-prefix? url "gopher://")
             (send browser-canvas go (url->request url))]
            [(or (string-prefix? url "http://")
                 (string-prefix? url "https://"))
             (send-url url #t)])
          ;; handle partial URLs
          (let ([base-req (url->request base-url)])
            (send browser-canvas go
                  (struct-copy request
                               base-req
                               [type (guess-type-from-filename url)]
                               [path/selector
                                (if (equal? (string-ref url 0) #\/)
                                    url
                                    (replace-final-path-element (request-path/selector base-req) url))])))))

    (define/override (on-event dc x y editorx editory event)
      (eprintf "html-link-snip% mouse event ~a~n" (send event get-event-type))
      (cond
        [(send event moving?)
         (eprintf "mouse motion event~n")
         (send browser-canvas update-status status-text)]
        [(send event button-down? 'left)
         (follow-link)]))

    (define/override (on-goodbye-event dc x y editorx editory event)
      ;(eprintf "goodbye event~n")
      (send browser-canvas update-status "Ready"))))

(define html-link-snip% (html-link-mixin string-snip%))
(define html-link-img-snip% (html-link-mixin image-snip%))
