#lang racket/gui

(require "gopher.rkt")

(provide browser-text%
         browser-canvas%
         menu-item-snip%
         goto-url)

(define (insert-menu-item text-widget line)
  (define (gopher-menu-type-text type)
    (case type
      [(#\0) "(TEXT) "]
      [(#\1) " (DIR) "]
      [(#\3) " (ERR) "]
      [(#\g) " (GIF) "]
      [(#\I) " (IMG) "]
      [(#\7) "(SRCH) "]
      [(#\8) " (TEL) "]
      [else  " (BIN) "]))

  (define standard-style
    (send (send text-widget get-style-list) find-named-style "Standard"))
  (define link-style
    (send (send text-widget get-style-list) find-named-style "Link"))
  
  (define selector (parse-selector line))
  (define type-snip (new string-snip%))
  (define link-snip
    (new menu-item-snip%
         (type (gopher-selector-item-type selector))
         (url (string-append "gopher://"
                             (gopher-selector-host selector) ":"
                             (gopher-selector-port selector)
                             (gopher-selector-path selector)))))
  
  (define type-text (gopher-menu-type-text (gopher-selector-item-type selector)))
  (define display-text (gopher-selector-text selector))
  
  (send type-snip set-style standard-style)
  (send type-snip insert type-text (string-length type-text))
  (send text-widget insert type-snip)
  (send link-snip set-style link-style)
  (send link-snip insert display-text (string-length display-text)) ;(send link-snip get-count))
  (send text-widget insert link-snip)
  (send text-widget change-style standard-style)
)

(define (insert-directory-line text-widget line)
  (cond
    [(not (non-empty-string? line))
     (send text-widget insert "\n")]
    ;; insert informational lines as plain text
    [(equal? (string-ref line 0) #\i)
     (define text (car (string-split (substring line 1) "\t" #:trim? #f)))
     (send text-widget insert text)
     (send text-widget insert "\n")]
    [else
     (insert-menu-item text-widget line)
     (send text-widget insert "\n")]))

;; by default the gopher item type is determined from the URL
(define (goto-url address-url page-text [type #f])
  (define resp (fetch address-url type))
  ;; default the item type to directory
  (define item-type (if (gopher-response-item-type resp)
                        (gopher-response-item-type resp)
                        #\1))

  ;; reset gopher-menu? boolean to default
  (set-field! gopher-menu? page-text #f)
  
  (cond
    [(gopher-response-error? resp)
     (send page-text erase)
     (send page-text insert (gopher-response-data resp))]
    [(equal? item-type #\1) ; directory
     (send page-text erase)
     (for ([line (in-lines (open-input-bytes (gopher-response-data resp)))])
       (insert-directory-line page-text line))
     (send page-text init-gopher-menu)]
    [(equal? (gopher-response-item-type resp) #\0)
     (send page-text erase)
     (send page-text insert (bytes->string/utf-8 (gopher-response-data resp)))]
    [else (void)]))

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
             scroll-to-position
             get-visible-position-range
             get-style-list
             change-style
             find-first-snip)

    (define/public (find-first-menu-snip)
      (define first-snip (find-first-snip))
      (if (is-a? first-snip menu-item-snip%)
          first-snip
          (find-next-menu-snip first-snip)))

    (define/public (init-gopher-menu)
      (set! gopher-menu? #t)
      (set! selection (find-first-menu-snip))
      (when selection
        (define new-style (send (get-style-list) find-named-style "Link Highlight"))
        (define pos (get-snip-position selection))
        (set-position pos 'same #f #t 'default)
        (change-style new-style
                      pos
                      (+ pos (send selection get-count)))
        (scroll-to-position 0)))

    (define/private (current-selection-visible?)
      (define pos (get-snip-position selection))
      (define start (box 0))
      (define end (box 0))
      (get-visible-position-range start end #f)
      (if (and (>= pos (unbox start)) (<= pos (unbox end)))
          #t
          #f))
    
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
             (cond
               [(or (not selection) (current-selection-visible?))
                ;; change selection to the next menu snip
                (define item (find-next-menu-snip selection))
                (eprintf "browser-text on-local-char down: new selection = ~a~n" item)
                (when item
                  (define pos (get-snip-position item))
                  (change-highlight selection item)
                  (set! selection item)
                  (set-position pos 'same #f #t 'default))]
               [else
                ;; else scroll to make the current selection visible
                (scroll-to-position (get-snip-position selection))])]
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
               (goto-url (get-field url selection)
                         this
                         (get-field type selection)))]
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
