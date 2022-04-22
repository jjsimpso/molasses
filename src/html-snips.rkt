#lang racket/base

(require racket/class
         racket/snip
         racket/format)

(provide horz-line-snip%)

(define horz-line-snip%
  (class snip%
    (super-new)

    (init-field [width-attribute '('percent . 100)]
                [align-attribute 'center]
                ; the left margin/horizontal inset of the editor-canvas<%>
                [horz-offset 5])

    (define (calc-width dc-width)
      ;(eprintf "get-extent width=~a~n" (* dc-width (/ (cdr width-attribute) 100)))
      (if (eq? (car width-attribute) 'pixels)
          (cdr width-attribute)
          (* (- dc-width (* horz-offset 2))
             (/ (cdr width-attribute) 100))))

    (define (calc-height)
      8.0)
    
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
      (maybe-set-box! h (calc-height))
      (maybe-set-box! descent 0.0)
      (maybe-set-box! space 0.0)
      (maybe-set-box! lspace 0.0)
      (maybe-set-box! rspace 0.0))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      
      (send dc set-pen "black" 1 'solid)
      (send dc set-smoothing 'smoothed)
      
      (define-values (w h) (send dc get-size))
      (define drawable-width (- w (* horz-offset 2)))
      (define width (calc-width w))
      (define height (calc-height))
      
      ;; draw debug rectangle
      #|
      (define old-brush (send dc get-brush))
      (send dc set-pen "blue" 1 'transparent)
      (send dc set-brush "green" 'solid)
      (send dc draw-rectangle x y (+ x drawable-width) height)
      ;(send dc draw-line x y (+ x drawable-width) y)
      (send dc set-brush old-brush)
      (send dc set-pen "black" 1 'solid)
      |#
      (define x-pos
        (cond
          [(eq? align-attribute 'left) x]
          [(eq? align-attribute 'right)
           (max (- w horz-offset width) 0)]
          [else
           ;; default to 'center
           (+ x (/ (max (- drawable-width width) 0) 2))]))
      (define y-pos (+ y (/ height 2)))
      ;(eprintf "draw: align=~a,w=~a,width=~a draw ~a to ~a~n" align-attribute w width x-pos (+ x-pos width))
      ;(eprintf "draw: draw rectangle ~a to ~a~n" x (+ x drawable-width))
      (send dc draw-line x-pos           y-pos
                         (+ x-pos width) y-pos)

      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen))

    (define/override (copy)
      (new horz-line-snip%
           [width-attribute width-attribute]
           [align-attribute align-attribute]))))
