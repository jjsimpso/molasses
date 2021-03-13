#lang racket/gui

(require "gopher.rkt"
         "widgets.rkt")

(provide init-new-tab
         tab-panel-callback)

;; holds the ui component and data for a tab
(struct tab-info
  (index
   contents
   address-text)
  #:prefab)

(define tab-list '())

(define (init-new-tab tp index)
  (printf "Init tab selection ~a~n" index)
  (send tp change-children
        (lambda (c*) '()))

  (define tab-contents
    (new vertical-panel% (parent tp)
         (alignment '(left top))
         (stretchable-width #t)
         (stretchable-height #t)
         (horiz-margin 0)))
    
  (define address-pane
    (new horizontal-pane% (parent tab-contents)
         (alignment '(left top))
         (stretchable-width #t)
         (stretchable-height #f)
         (horiz-margin 10)))

  (define back-key
    (new button% (parent address-pane)
         (label "\u2190") ; Back arrow
         (enabled #f)
         (horiz-margin 0)
         #;(callback (λ _ (go-back)))))
  (define forward-key
    (new button% (parent address-pane)
         (label "\u2192") ; Forward arrow
         (enabled #f)
         (horiz-margin 0)
         #;(callback (λ _ (go-forward)))))
  (define address-field
    (new text-field% (parent address-pane)
         (label "")
         (init-value "gopher://gopher.endangeredsoft.org")
         (style '(single))
         (callback
          (lambda (item event)
            (when (equal? (send event get-event-type)
                          'text-field-enter)
              (goto-url (send item get-value) page-text)
              (send page-canvas focus))))))

  (define page-text
    (new browser-text%))

  (define page-canvas
    (new browser-canvas% (parent tab-contents)
         (editor page-text)
         (style '(auto-hscroll auto-vscroll))
         (wheel-step 3)))

  (init-styles (send page-text get-style-list))

  (send page-text set-max-undo-history 0)
  ;(send page-text hide-caret #t)
  (send* page-canvas
    (set-canvas-background (make-object color% "black"))
    (force-display-focus #t)
    (lazy-refresh #t))

  (set! tab-list
        (cons (cons index tab-contents)
              tab-list)))

(define (init-styles style-list)
  (define standard (send style-list find-named-style "Standard"))
  (define standard-delta (make-object style-delta%))
  (send* standard-delta
    (set-family 'modern)
    ;(set-face font-name)
    (set-delta 'change-size 11)
    (set-delta-foreground (make-object color% "white"))
    (set-delta-background (make-object color% "black")))
  (send standard set-delta standard-delta)

  (define (make-color-style name color)
    ;; Each style created with this procedure copies "Standard" style
    ;; and creates a new style by name 'name' and with the foreground
    ;; color 'color'.
    (send (send style-list new-named-style name standard)
          set-delta (send* (make-object style-delta%)
                      (copy standard-delta)
                      (set-delta-foreground color))))

  (make-color-style "Link" (make-object color% "green"))
)


(define (change-tab tp event)
  (when (eq? (send event get-event-type) 'tab-panel)
    (define tab-index (send tp get-selection))
    (define tab-label (send tp get-item-label tab-index))
    (printf "changing to tab ~a~n" tab-label)
    (fill-tab-content tp)))

(define (fill-tab-content tp)
  (define current-tab-index (send tp get-selection))
  (send tp change-children
        (lambda (c*)
          (list (cdr (assoc current-tab-index tab-list))))))

(define (tab-panel-callback item event)
  (define tab-index (send item get-selection))
  (define tab-label (send item get-item-label tab-index))

  (if (string=? tab-label "+")
      (begin
        (send item append "New")
        (send item delete tab-index)
        (send item append "+")
        (init-new-tab item tab-index))
      (change-tab item event)))

(define (insert-menu-item text-widget line)
  (define (gopher-menu-type-text type)
    (case type
      [(#\0) "(TEXT) "]
      [(#\1) "(DIR)  "]
      [(#\3) "(ERROR) "]
      [(#\g) "(GIF)  "]
      [(#\I) "(IMG)  "]
      [(#\7) "(SEARCH) "]
      [(#\8) "(TELNET) "]
      [else  "(BIN)  "]))

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

(define (goto-url address-url page-text)
  ;; find the text% widget for the tab
  #;(define (get-page-text addr-field)
    (define tab-contents-children
      (send (send (send addr-field get-parent) get-parent) get-children))
    (send (second tab-contents-children) get-editor))
  (define resp (fetch address-url))
  ;; default the item type to directory
  (define item-type (if (gopher-response-item-type resp)
                        (gopher-response-item-type resp)
                        #\1))
  (cond
    [(gopher-response-error? resp)
     (send page-text erase)
     (send page-text insert (gopher-response-data resp))]
    [(equal? item-type #\1) ; directory
     (send page-text erase)
     (for ([line (in-lines (open-input-bytes (gopher-response-data resp)))])
       (insert-directory-line page-text line))
     (set-field! selection page-text (send page-text find-first-snip))]
    [(equal? (gopher-response-item-type resp) #\0)
     (send page-text erase)
     (send page-text insert (bytes->string/utf-8 (gopher-response-data resp)))]
    [else (void)]))
