#lang racket/gui

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
         (style '(single))))
         
  (define page-text
    (new text%))

  (define page-canvas
    (new editor-canvas% (parent tab-contents)
         (editor page-text)
         (style '(no-focus auto-hscroll auto-vscroll))
         (wheel-step 3)))

  (send page-text set-max-undo-history 0)
  (send* page-canvas
    (set-canvas-background (make-object color% "black"))
    (force-display-focus #t)
    (lazy-refresh #t))

  (set! tab-list
        (cons (cons index tab-contents)
              tab-list)))

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
