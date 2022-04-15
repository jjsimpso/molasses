#lang racket/base

(require racket/class
         racket/snip
         racket/format)

(provide horz-line-snip%)

(define horz-line-snip%
  (class snip%
    (super-new)

    (define/override (get-extent dc x y	 	 	 	 
                                   [w #f]
                                   [h #f]
                                   [descent #f]
                                   [space #f]
                                   [lspace #f]
                                   [rspace #f])
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (define-values (width height) (send dc get-size))
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
      (define y-pos (+ y 3.0))
      (send dc draw-line (+ x 2.0)       y-pos
                         (- (+ x w) 2.0) y-pos)

      (send dc set-smoothing old-smoothing)
      (send dc set-pen old-pen))

    (define/override (copy)
      (new horz-line-snip%))))
