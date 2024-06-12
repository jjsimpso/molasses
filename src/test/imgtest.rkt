#lang racket/gui

(require framework)
(require "../html-snips.rkt")

(define frame 
  (new (class frame% (super-new))
       [label "IMG test"]
       [width 800]
       [height 600]))

(define editor
  (new ;(text:inline-overview-mixin
        (class text%
          (super-new))))

(define canvas
  (new editor-canvas% (parent frame)
       (editor editor)))

(send frame show #t)

(define img (new img-hack-snip%
                 [in "/data/jonathan/reversing/Fravia/cracdumm.gif"]
                 [align 'right]))
(define space (make-object string-snip% " "))
(send space set-flags (cons 'hard-newline (send space get-flags)))

;(send editor set-inline-overview-enabled? #t)
(send editor auto-wrap #t)
(for ([i (in-range 1 5)])
  (send editor insert "Hello world\n"))
(send editor insert img)
(send editor insert space)
(send editor insert "\n")
(define text (make-object string-snip% "He is immortal. Born in the highlands of Scotland 400 years ago, there are others like him. Some good, some evil. For centuries he has battled the forces of darkness with holy ground his only refuge. He cannot die unless you take his head and with it his power. There can be only one. He is Duncan Macleod, the Highlander!\n"))
(define style (send text get-style))
(eprintf "style font=~a, fg=~a, bg=~a~n" (send style get-font) (send style get-foreground) (send style get-background))
(send img push (send text get-text 0 999) style 'left)
;(send editor insert space)
;(send editor insert "\n")
(send editor insert (new horz-line-snip%))
(send editor insert "text\nwith lots\nof\nnewlines")


; don't run this file for testing:
(module test racket/base)
