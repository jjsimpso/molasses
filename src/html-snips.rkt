#lang racket/base

(require racket/class
         racket/snip
         racket/format)

(provide horz-line-snip%)

(define horz-line-snip%
  (class snip%
    (super-new)

    (init-field [width-attribute '('percent . 100)]
                [align-attribute 'center])

    (define (calc-width dc-width)
      ;(eprintf "get-extent width=~a~n" (* dc-width (/ (cdr width-attribute) 100)))
      (if (eq? (car width-attribute) 'pixels)
          (cdr width-attribute)
          (* dc-width
             (/ (cdr width-attribute) 100))))
      
    (define/override (get-extent dc x y	 	 	 	 
                                   [w #f]
                                   [h #f]
                                   [descent #f]
                                   [space #f]
                                   [lspace #f]
                                   [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (define-values (width height) (send dc get-size))
      
      ;(maybe-set-box! w (calc-width width))
      (maybe-set-box! w width)
      (maybe-set-box! h 6.0)
      (maybe-set-box! descent 0.0)
      (maybe-set-box! space 4.0)
      (maybe-set-box! lspace 0.0)
      (maybe-set-box! rspace 0.0))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define old-pen (send dc get-pen))
      (define old-smoothing (send dc get-smoothing))
      
      (send dc set-pen "black" 1 'solid)
      (send dc set-smoothing 'smoothed)
      
      (define-values (w h) (send dc get-size))
      (define width (calc-width w))
      (eprintf "draw: align=~a,width=~a, at (~a,~a)~n" align-attribute width x y)
      (define x-pos
        (cond
          [(eq? align-attribute 'left) x]
          [(eq? align-attribute 'right)
           (max (- w width) 0)]
          [else
           ;; default to 'center
           (+ x (/ (max (- w width) 0) 2))]))
      (define y-pos (+ y 3.0))
      (send dc draw-line x-pos           y-pos
                         (+ x-pos width) y-pos)

      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen))

    (define/override (copy)
      (new horz-line-snip%
           [width-attribute width-attribute]
           [align-attribute align-attribute]))))
