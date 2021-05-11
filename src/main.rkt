#lang racket/gui

(require "tab.rkt")

(define frame 
  (new
   (class frame% (super-new)
     (define (handle-keycombo key)
       (let ([ctrl? (send key get-control-down)]
             [meta? (send key get-meta-down)]
             [key-code (send key get-key-code)])
         (cond
           #;((eq? key-code 'f5)
            (refresh))
           #;((eq? key-code 'escape)
            (send search-panel show #f))
           #;((and meta? (eq? key-code 'home))
              (go-home))
           [(and ctrl? (eq? key-code 'prior))
            (prev-tab tab-panel)]
           [(and ctrl? (eq? key-code 'next))
            (next-tab tab-panel)]
           [(and ctrl? (eq? key-code #\l))
            (define address-field (find-tp-address-field tab-panel))
            (send address-field focus)
            (send (send address-field get-editor) select-all)]
           ;; Return #f if we don't recognise this key code so that it can be
           ;; delegated to lower levels in on-subwindow-char (such as the
           ;; canvas or the text).
           (else
            #f))))

     (define/augment (on-close)
       (save-tabs tab-panel))

     (define/override (on-subwindow-char receiver event)
       (or (handle-keycombo event)
           (send this on-menu-char event)
           (send this on-system-menu-char event)
           (send this on-traverse-char event))))
   
   [label "Molasses"]
   [width 1200]
   [height 800]))

(define (find-item-editor item)
  (let ([o (let loop ([i item])
             (let ([p (send i get-parent)])
               (cond
                 [(not p) #f]
                 [(is-a? p popup-menu%)
                  (let ([p (send p get-popup-target)])
                    (if (is-a? p window<%>)
                        (let ([f (send p get-top-level-window)])
                          (and f (send f get-edit-target-object)))
                        p))]
                 [(is-a? p menu%) (loop p)]
                 [else (let ([f (send p get-frame)])
                         (and f (send f get-edit-target-object)))])))])
    (and (is-a? o editor<%>)
         o)))

(define menu-bar (new menu-bar% (parent frame)))
(define file-menu 
  (new menu%
       (label "&File")
       (parent menu-bar)))
(define edit-menu
  (new menu%
       (label "&Edit")
       (parent menu-bar)))
#;(define font-menu
  (new menu%
       [label "Font"]
       [parent menu-bar]))
(define help-menu
  (new menu%
       (label "&Help")
       (parent menu-bar)))

(new menu-item%
     (label "Copy")
     (parent edit-menu)
     (callback 
      (lambda (item event)
        (define o (find-item-editor item))
        (when o
          (send o do-edit-operation 'copy)))))

(new (class menu-item% (super-new)
       (inherit enable)
       ;; disable the paste menu item if browser-text% has focus
       (define/override (on-demand)
         (let ([o (find-item-editor this)])
           (enable (and o (send o can-do-edit-operation? 'paste))))))
     (label "Paste")
     (parent edit-menu)
     (callback 
      (lambda (item event)
        (define o (find-item-editor item))
        (when o
          (send o do-edit-operation 'paste)))))

(new menu-item%
     (label "Select All")
     (parent edit-menu)
     (callback 
      (lambda (item event)
        (define o (find-item-editor item))
        (when o
          (send o do-edit-operation 'select-all)))))

(new menu-item%
     (label "Exit")
     (parent file-menu)
     (callback 
      (lambda (item event) (send frame on-exit))))

;(append-editor-font-menu-items font-menu)

(define tab-panel 
  (new tab-panel%
       (parent frame)
       ;(style '(can-close))
       (callback tab-panel-callback)
       (choices '())))

(define status-bar
  (new message%
       (parent frame)
       (label "Ready")
       (stretchable-width #t)))

;; load any saved tabs, else initialize one new tab
(unless (load-tabs tab-panel)
  (send tab-panel append "New")        
  (init-new-tab tab-panel 0)
  (goto-home-page))

;; this tab is actually just used as a button
(send tab-panel append "+")

;(send tab-panel set-selection 0)

;; Select means copy for X11.
(editor-set-x-selection-mode #t)

;(application-quit-handler quit)
;(application-about-handler about)


; Show the frame by calling its show method
(send frame show #t)

