#lang racket/gui

(provide find-panel%)

(define find-panel%
  (class horizontal-panel%
    (super-new)
    (init [find-canvas #f])
    (inherit get-parent)
    
    (define canvas find-canvas)
    (define find-results-number 0)
    (define find-results-index 0)

    #;(define/override (show show?)
      (unless show?
        (close))
      (super show show?))

    (define/public (visible?)
      (member this (send (get-parent) get-children)))

    (define/override (focus)
      (send find-text-field focus))
    
    (define/public (close)
      (send find-text-field set-value "")
      (send find-results-msg set-label "0 items found")
      (set! find-results-number 0)
      (set! find-results-index 0)
      (send canvas find-results-clear)
      (send (get-parent) delete-child this)
      (send canvas focus))
  
    (define (find-text-callback item event)
      (case (send event get-event-type)
        [(text-field-enter)
         (printf "searching for ~a~n" (send find-text-field get-value))
         (send canvas find-results-clear)
         (send canvas find-in-canvas (send find-text-field get-value) (send find-case-checkbox get-value))
         (set! find-results-number (send canvas find-results-length))
         (set! find-results-index 0)
         (send canvas refresh)
         (if (> find-results-number 0)
             (send find-results-msg set-label (format "0 of ~a items found" find-results-number))
             (send find-results-msg set-label (format "0 items found")))]
        [(text-field)
         void]))

    (define find-text-field
      (new text-field%
           (label "Find")
           (parent this)
           (stretchable-width #f)
           (callback
            (lambda (item event)
              (find-text-callback item event)))))

    (define find-results-prev
      (new button%
           (label "<-")
           (parent this)
           (stretchable-width #f)
           (callback
            (lambda (item event)
              (when (send canvas find-results-prev)
                (send canvas refresh)
                (set! find-results-index (sub1 find-results-index))
                (send find-results-msg set-label (format "~a of ~a items found" find-results-index find-results-number)))))))

    (define find-results-next
      (new button%
           (label "->")
           (parent this)
           (stretchable-width #f)
           (callback
            (lambda (item event)
              (when (send canvas find-results-next)
                (send canvas refresh)
                (set! find-results-index (add1 find-results-index))
                (send find-results-msg set-label (format "~a of ~a items found" find-results-index find-results-number)))))))

    (define find-results-msg
      (new message%
           (label "0 items found")
           (parent this)
           (auto-resize #t)
           (stretchable-width #f)))

    (define find-panel-right-pane
      (new horizontal-pane%
           (parent this)
           (alignment '(right center))
           (stretchable-width #t)))

    (define find-case-checkbox
      (new check-box%
           (label "Match case")
           (parent find-panel-right-pane)
           (callback
            (lambda (item event)
              (find-text-callback find-text-field (new control-event% (event-type 'text-field-enter)))))))
  
    (define find-panel-close-button
      (new button%
           (label "Close")
           (parent find-panel-right-pane)
           (stretchable-width #f)
           (callback
            (lambda (item event)
              (close)))))
))
