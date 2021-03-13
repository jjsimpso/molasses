#lang racket/gui

(provide browser-text%
         browser-canvas%
         menu-item-snip%)

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

(define browser-text%
  (class text% (super-new)
    (init-field [selection #f]
                [gopher-menu? #f])
    (inherit get-snip-position
             set-position
             move-position
             get-style-list
             change-style)
    
    (define/private (change-highlight old-sel new-sel)
      (when old-sel
        ;; unhighlight previous selection
        (define old-style (send (get-style-list) find-named-style "Link"))
        (define pos (get-snip-position old-sel))
        (change-style old-style
                      pos
                      (+ pos (send old-sel get-count))))
      (define new-style (send (get-style-list) find-named-style "Link Highlight"))
      (define pos (get-snip-position new-sel))
      (change-style new-style
                    pos
                    (+ pos (send new-sel get-count))))
    
    (define/override (on-local-char event)
      (if (not gopher-menu?)
          (super on-local-char event)
          (case (send event get-key-code)
            [(down)
             (define item (find-next-menu-snip selection))
             (eprintf "browser-text on-local-char down: new selection = ~a~n" item)
             (when item
               (define pos (get-snip-position item))
               (change-highlight selection item)
               (set! selection item)
               (set-position pos 'same #f #t 'default))]
            [(up)
             (define item (find-prev-menu-snip selection))
             (eprintf "browser-text on-local-char up: new selection = ~a~n" item)
             (when item
               (define pos (get-snip-position item))
               (change-highlight selection item)
               (set! selection item)
               (set-position pos 'same #f #t 'default))]
            [(left)
             (void)]
            [(right)
             (when selection
               (send selection follow-link))]
            [(next prior)
             (eprintf "browser-text on-local-char: got page up/down key~n")
             (move-position (send event get-key-code))]
            [else
             (define key-code (send event get-key-code))
             (eprintf "browser-text on-local-char: unhandled key ~a~n" key-code)
             (void)])))
    ))

(define browser-canvas%
  (class editor-canvas% (super-new)
    (init-field [selection #f])
    (inherit get-editor)
    ))

(define menu-item-snip%
  (class string-snip%
    (init-field [url ""]
                [type #\1])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    ;(set-flags (cons 'handles-events (get-flags)))

    (define/public (follow-link)
      (eprintf "follow-link: ~a~n" url))
    
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down? 'left)
        (follow-link)))))
