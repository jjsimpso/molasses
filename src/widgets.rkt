#lang racket/gui

(provide browser-canvas% menu-item-snip%)

(define (find-next-menu-snip snip)
  (if (not snip)
      #f
      (let ([next-snip (send snip next)])
        ;(eprintf "find-next-menu-snip: ~a, ~a~n" snip next-snip)
        (if (is-a? next-snip menu-item-snip%)
            next-snip
            (find-next-menu-snip next-snip)))))

(define (find-prev-menu-snip snip)
  (if (not snip)
      #f
      (let ([prev-snip (send snip previous)])
        ;(eprintf "find-prev-menu-snip: ~a, ~a~n" snip prev-snip)
        (if (is-a? prev-snip menu-item-snip%)
            prev-snip
            (find-prev-menu-snip prev-snip)))))

(define browser-canvas%
  (class editor-canvas% (super-new)
    (init-field [selection #f])
    (inherit get-editor)
    (define/override (on-char event)
      (case (send event get-key-code)
        [(wheel-up wheel-down)
         (super on-char event)]
        [(down)
         (define text-widget (get-editor))
         (define item (find-next-menu-snip selection))
         (eprintf "browser on-char down: new selection = ~a~n" item)
         (when item
           (define pos (send text-widget get-snip-position item))
           (set! selection item)
           (send text-widget set-position pos 'same #f #t 'default))]
        [(up)
         (define text-widget (get-editor))
         (define item (find-prev-menu-snip selection))
         (eprintf "browser on-char up: new selection = ~a~n" item)
         (when item
           (define pos (send text-widget get-snip-position item))
           (set! selection item)
           (send text-widget set-position pos 'same #f #t 'default))]
        [(left right next prior)
         (eprintf "browser on-char: got arrow key~n")
         (define text-widget (get-editor))
         (send text-widget move-position (send event get-key-code))
         #;(super on-char event)]
        [else
         (define key-code (send event get-key-code))
         (eprintf "browser on-char: unhandled key ~a~n" key-code)
         (void)]))
    ))

(define menu-item-snip%
  (class string-snip%
    (init-field [url ""]
                [type #\1])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    (set-flags (cons 'handles-events (get-flags)))
    (define/override (on-char dc x y edx edy event)
      (case (send event get-key-code)
        [(#\return) (follow-link url type)]
        [else #f]))
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down? 'left)
        (follow-link url type)))))

(define (follow-link url-string type)
  (eprintf "follow-link:~n")
)
