#lang racket/gui

(provide browser-canvas%)

(define browser-canvas%
  (class editor-canvas% (super-new)
    
    (define/override (on-char event)
      (case (send event get-key-code)
        [(wheel-up wheel-down)
         (super on-char event)]
        [(left right up down next prior)
         (eprintf "browser on-char: got arrow key~n")
         (super on-char event)]
        [else
         (define key-code (send event get-key-code))
         (eprintf "browser on-char: unhandled key ~a~n" key-code)
         (void)]))
    ))

(define menu-item-snip%
  (class string-snip%
    (init-field [url ""])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down? 'left)
        (follow-link url)))))

(define (follow-link url-string)
  (void))
