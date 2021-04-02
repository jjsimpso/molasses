#lang racket/gui

(require "widgets.rkt")
(require "config.rkt")

(provide init-new-tab
         tab-panel-callback)

;; holds the ui component and data for a tab
(struct tab-info
  (index
   contents
   address-text)
  #:prefab)

;; list of tab-info structs
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

  (define back-button
    (new button% (parent address-pane)
         (label "\u2190") ; Back arrow
         (enabled #t)
         (horiz-margin 0)
         (callback
          (lambda (item event)
            (send page-text go-back)))))
  
  (define forward-button
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
              (send page-text go (send item get-value) #f)
              (send page-canvas focus))))))

  (define page-text
    (new browser-text% (address-text-field address-field)))

  (define page-canvas
    (new browser-canvas% (parent tab-contents)
         (editor page-text)
         (style '(auto-hscroll auto-vscroll))
         (wheel-step 3)))

  (init-styles (send page-text get-style-list))

  (send page-text set-max-undo-history 0)
  (send page-text set-styles-sticky #f)
  
  (send page-text hide-caret #t)
  (send* page-canvas
    (set-canvas-background canvas-bg-color)
    (force-display-focus #t)
    (lazy-refresh #t))

  (set! tab-list
        (cons (tab-info index tab-contents (send address-field get-value))
              tab-list)))

(define (init-styles style-list)
  (define standard (send style-list find-named-style "Standard"))
  (define standard-delta (make-object style-delta%))
  (send* standard-delta
    (set-family 'modern)
    ;(set-face font-name)
    (set-delta 'change-size 12)
    (set-delta-foreground text-fg-color)
    (set-delta-background canvas-bg-color))
  (send standard set-delta standard-delta)

  (define (make-color-style name color)
    ;; Each style created with this procedure copies "Standard" style
    ;; and creates a new style by name 'name' and with the foreground
    ;; color 'color'.
    (send (send style-list new-named-style name standard)
          set-delta (send* (make-object style-delta%)
                      (copy standard-delta)
                      (set-delta-foreground color))))

  (make-color-style "Link" link-color)
  (make-color-style "Link Highlight" link-highlight-color)
)

;; returns a tab-info struct from the global tab-list or #f
(define (find-tab-at-index index)
  (for/first ([tab (in-list tab-list)]
              #:when (= index (tab-info-index tab)))
    tab))

;; returns the tab at index's canvas widget or #f
(define (find-tab-canvas index)
  (define tab (find-tab-at-index index))
  (when tab
    (define children (send (tab-info-contents tab) get-children))
    (eprintf "tab children: ~a~n" children)
    (for/first ([child (in-list children)]
                #:when (is-a? child browser-canvas%))
      child)))

(define (change-tab tp event)
  (when (eq? (send event get-event-type) 'tab-panel)
    (define tab-index (send tp get-selection))
    (define tab-label (send tp get-item-label tab-index))
    (printf "changing to tab ~a~n" tab-label)
    (fill-tab-content tp)
    (define tab-canvas (find-tab-canvas tab-index))
    (when tab-canvas
      (send tab-canvas focus))))

(define (fill-tab-content tp)
  (define current-tab-index (send tp get-selection))
  (send tp change-children
        (lambda (c*)
          ;; return contents of the tab as a list
          (list (tab-info-contents (find-tab-at-index current-tab-index))))))

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
