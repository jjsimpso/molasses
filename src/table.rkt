#lang racket/gui

(require racket/class
         racket/snip
         racket/draw)

(provide table-snip%)

(define table-snip%
  (class snip% (super-new)
    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      void)

    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned])
      void)))

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

  (define table (new table-snip%))
  (send canvas append-snip table)
  
  (send frame show #t))
