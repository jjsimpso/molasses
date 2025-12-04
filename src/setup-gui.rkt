#lang racket/gui

(require "const.rkt"
         "config.rkt"
         "tab.rkt"
         "request.rkt"
         "download.rkt"
         "bookmark.rkt"
         "widgets.rkt")

(provide setup-gui)

(define (setup-gui)
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
             [(and ctrl? (eq? key-code #\t))
              (new-tab tab-panel)]
             [(and meta? (eq? key-code #\=))
              (define canvas (active-page-canvas tab-panel))
              (when canvas
                (change-canvas-horizontal-inset canvas 50))]
             [(and meta? (eq? key-code #\-))
              (define canvas (active-page-canvas tab-panel))
              (when canvas
                (change-canvas-horizontal-inset canvas -50))]
             ;; Return #f if we don't recognise this key code so that it can be
             ;; delegated to lower levels in on-subwindow-char (such as the
             ;; canvas or the text).
             (else
              #f))))

       (define (save-settings)
         (put-preferences (list 'smooth-scrolling) (list canvas-smooth-scrolling) #f pref-file))
       
       (define/augment (on-close)
         (save-settings)
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
      (and (or (is-a? o editor<%>) (is-a? o browser-canvas%))
           o)))

  (define (change-canvas-font-size canvas amount)
    (define standard (send (send canvas get-style-list) find-named-style "Standard"))
    (when standard
      (define delta (make-object style-delta%))
      (send standard get-delta delta)
      (define additive-factor (max 0 (+ (send delta get-size-add) amount)))
      (send delta set-delta 'change-bigger additive-factor)
      ;(printf "increasing font size to ~a~n" (send delta get-size-add))
      (send standard set-delta delta)
      (send canvas redo-layout)))

  (define (change-canvas-horizontal-inset canvas amount)
    (define inset (send canvas horizontal-inset))
    (send canvas set-horizontal-inset (max 0 (+ inset amount))))
  
  (define (set-light-mode canvas on?)
    (define (set-style-color name fg-color bg-color)
      (define style (send (send canvas get-style-list) find-named-style name))
      (when style
        ;(printf "setting ~a's light mode to ~a~n" name on?)
        (define delta (make-object style-delta%))
        (send style get-delta delta)
        (send* delta
          (set-delta-foreground fg-color)
          (set-delta-background bg-color))
        (send style set-delta delta)))

    (send canvas begin-edit-sequence)
    (if on?
        (begin
          (set-field! default-bg-color canvas light-canvas-bg-color)
          (send canvas set-canvas-background light-canvas-bg-color)
          (set-style-color "Standard" light-text-fg-color light-text-bg-color)
          (set-style-color "Link" light-link-color light-text-bg-color)
          (set-style-color "Link Highlight" light-link-highlight-color light-text-bg-color))
        (begin
          (set-field! default-bg-color canvas canvas-bg-color)
          (send canvas set-canvas-background canvas-bg-color)
          (set-style-color "Standard" text-fg-color text-bg-color)
          (set-style-color "Link" link-color text-bg-color)
          (set-style-color "Link Highlight" link-highlight-color text-bg-color)))
    (send canvas end-edit-sequence))
  
  (define menu-bar (new menu-bar% (parent frame)))
  (define file-menu 
    (new menu%
         (label "&File")
         (parent menu-bar)))
  (define edit-menu
    (new menu%
         (label "&Edit")
         (parent menu-bar)))
  (define tab-menu
      (new menu%
           [label "&Tab"]
           [parent menu-bar]))
  (define options-menu
    (new menu%
         (label "&Options")
         (parent menu-bar)))
  (define bookmark-menu
    (new menu%
         (label "&Bookmarks")
         (parent menu-bar)))
  (define help-menu
    (new menu%
         (label "&Help")
         (parent menu-bar)))

  (new menu-item%
       (label "Copy")
       (parent edit-menu)
       (shortcut #\C)
       (shortcut-prefix '(ctl))
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
       (shortcut #\V)
       (shortcut-prefix '(ctl))
       (callback 
        (lambda (item event)
          (define o (find-item-editor item))
          (when o
            (send o do-edit-operation 'paste)))))

  (new menu-item%
       (label "Select All")
       (parent edit-menu)
       (shortcut #\A)
       (shortcut-prefix '(ctl))
       (callback 
        (lambda (item event)
          (define o (find-item-editor item))
          (when o
            (send o do-edit-operation 'select-all)))))

  (new menu-item%
       (label "Find in Page")
       (parent edit-menu)
       (shortcut #\F)
       (shortcut-prefix '(ctl))
       (callback 
        (lambda (item event)
          (if (member find-panel (send frame get-children))
              ; close find ui
              (let ([canvas (active-page-canvas tab-panel)])
                (send find-text-field set-value "")
                (send find-results-msg set-label "0 items found")
                (send frame delete-child find-panel)
                (set! find-results-number 0)
                (set! find-results-index 0)
                (when canvas
                  (send canvas find-results-clear)
                  (send canvas focus)))
              ; open find ui
              (begin
                (send frame change-children
                      (lambda (children)
                        (define-values (front end) (split-at-right children 1))
                        (append front (cons find-panel end))))
                (send find-text-field focus))))))

  (new checkable-menu-item%
       (label "Word Wrap")
       (parent tab-menu)
       (demand-callback
        (lambda (item)
          (define canvas (active-page-canvas tab-panel))
          (when canvas
            #;(eprintf "word wrap demand callback~n")
            ; disable word wrapping selection if in layout mode
            (if (eq? (send canvas get-mode) 'layout)
                (send item enable #f)
                (begin
                  (send item enable #t)
                  (send item check (eq? (send canvas get-mode) 'wrapped)))))))
       (callback 
        (lambda (item event)
          (define canvas (active-page-canvas tab-panel))
          (when canvas
            #;(eprintf "word wrap callback~n")
            (define checked? (send item is-checked?))
            (send canvas set-mode (if checked? 'wrapped 'plaintext))))))

  (new menu-item%
       (label "Increase Font Size")
       (parent tab-menu)
       (shortcut #\=)
       (shortcut-prefix '(ctl))
       (callback 
        (lambda (item event)
          (define canvas (active-page-canvas tab-panel))
          (when canvas
            (change-canvas-font-size canvas 1)))))

  (new menu-item%
       (label "Decrease Font Size")
       (parent tab-menu)
       (shortcut #\-)
       (shortcut-prefix '(ctl))
       (callback 
        (lambda (item event)
          (define canvas (active-page-canvas tab-panel))
          (when canvas
            (change-canvas-font-size canvas -1)))))

  (new checkable-menu-item%
       (label "Smooth Scrolling")
       (parent options-menu)
       (demand-callback
        (lambda (item)
          (if canvas-smooth-scrolling
              (send item check #t)
              (send item check #f))))
       (callback 
        (lambda (item event)
          (define checked? (send item is-checked?))
          (set-canvas-smooth-scrolling! checked?)
          (define canvas-list (get-all-tab-canvases))
          (for ([canvas (in-list canvas-list)])
            (set-field! smooth-scrolling canvas checked?)))))

  (new checkable-menu-item%
       (label "Light Mode")
       (parent options-menu)
       (demand-callback
        (lambda (item)
          (if canvas-light-mode
              (send item check #t)
              (send item check #f))))
       (callback 
        (lambda (item event)
          (define checked? (send item is-checked?))
          (set-canvas-light-mode! checked?)
          (define canvas-list (get-all-tab-canvases))
          (for ([canvas (in-list canvas-list)])
            (set-light-mode canvas checked?)))))
  

  ;(append-editor-font-menu-items font-menu)

  (new menu-item%
       (label "Exit")
       (parent file-menu)
       (callback 
        (lambda (item event) (send frame on-exit))))

  (new menu-item%
       (label "Introduction")
       (parent help-menu)
       (callback
        (lambda (item event)
          (open-help-tab tab-panel))))

  (new menu-item%
       (label "About Molasses")
       (parent help-menu)
       (callback
        (lambda (item event)
          (define dialog
            (new dialog% (parent frame)
                 (label "About Molasses")
                 (spacing 10)
                 (border 10)))
          (define panel
            (new horizontal-panel% (parent dialog)
                 (alignment '(center center))
                 (spacing 10)))
          #;(new message% (parent panel)
                 (label about-icon))
          (new message% (parent panel)
               (label about-version-string))
          (new button% (parent dialog)
               (label "OK")
               (callback (lambda _ (send dialog show #f))))
          (send dialog show #t))))

  ;; callback function for bookmark menu items
  (define (goto-bookmark url)
    #;(eprintf "opening bookmark to ~a~n" url)
    (define page-canvas (active-page-canvas tab-panel))
    (send page-canvas go (url->request url))
    (send page-canvas focus))

  (new menu-item%
       (label "Bookmark Page")
       (parent bookmark-menu)
       (callback
        (lambda (item event)
          (define address-field (find-tp-address-field tab-panel))
          (when address-field
            (define url (send address-field get-value))
            (define label (get-text-from-user "Add Bookmark"
                                              "Label:"
                                              frame
                                              url))
            (when label
              (add-bookmark url label bookmark-menu goto-bookmark))))))

  (new menu-item%
       (label "Home")
       (parent bookmark-menu)
       (callback (lambda (item event)
                   (goto-home-page tab-panel))))

  (new separator-menu-item%
       (parent bookmark-menu))

  ;; Add bookmarks to the Bookmark menu

  (load-bookmarks)

  (populate-bookmark-menu bookmark-menu goto-bookmark)

  ;; Create Tab Panel and status bars

  (define molasses-tab-panel%
    (class tab-panel%
      (super-new)

      (define/augment (on-reorder former-indices)
        #;(eprintf "on-reorder: enter~n")
        (update-tab-order this former-indices))
      
      (define/override (on-close-request index)
        #;(eprintf "on-close-request: enter~n")
        (delete-tab this index))

      (define/override (on-new-request)
        #;(eprintf "on-new-request: enter~n")
        (new-tab this))))

  (define tab-panel 
    (new molasses-tab-panel%
         (parent frame)
         (style '(flat-portable no-border new-button can-close can-reorder))
         (callback tab-panel-callback)
         (choices '())))

  (define find-results-number 0)
  (define find-results-index 0)
  
  (define find-panel
    (new horizontal-panel%
         (parent frame)
         (style '(deleted))
         (stretchable-height #f)))

  (define find-text-field
    (new text-field%
         (label "Find")
         (parent find-panel)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (case (send event get-event-type)
              [(text-field-enter)
               (define canvas (active-page-canvas tab-panel))
               (when canvas
                 (printf "searching for ~a~n" (send find-text-field get-value))
                 (send canvas find-in-canvas (send find-text-field get-value))
                 (set! find-results-number (send canvas find-results-length))
                 (set! find-results-index 0)
                 (send canvas refresh)
                 (if (> find-results-number 0)
                     (send find-results-msg set-label (format "0 of ~a items found" find-results-number))
                     (send find-results-msg set-label (format "0 items found"))))]
              [(text-field)
               void])))))

  (define find-results-msg
    (new message%
         (label "0 items found")
         (parent find-panel)
         (auto-resize #t)
         (stretchable-width #f)))

  (define find-results-prev
    (new button%
         (label "<-")
         (parent find-panel)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (define canvas (active-page-canvas tab-panel))
            (when canvas
              (when (send canvas find-results-prev)
                (set! find-results-index (sub1 find-results-index))
                (send find-results-msg set-label (format "~a of ~a items found" find-results-index find-results-number))))))))

  (define find-results-next
    (new button%
         (label "->")
         (parent find-panel)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (define canvas (active-page-canvas tab-panel))
            (when canvas
              (when (send canvas find-results-next)
                (set! find-results-index (add1 find-results-index))
                (send find-results-msg set-label (format "~a of ~a items found" find-results-index find-results-number))))))))
  
  (define status-bar
    (new horizontal-pane%
         (parent frame)
         (stretchable-height #f)))

  (define status-msg
    (new message%
         (parent status-bar)
         (label "Ready")
         (stretchable-width #t)))

  (define download-status
    (new download-panel%
         (parent status-bar)
         (border 2)
         (stretchable-width #f)
         (stretchable-height #f)))

  (define download-text
    (new message%
         (parent download-status)
         (label "Downloads")
         (stretchable-width #f)))

  (set-canvas-smooth-scrolling! (get-preference 'smooth-scrolling (lambda () #t) 'timestamp pref-file))
  
  ;; load any saved tabs, else initialize two tabs, one open to the
  ;; molasses home page and one containing the introductory help
  ;; text
  (unless (load-tabs tab-panel)
    (send tab-panel append "Home")
    (init-new-tab tab-panel 0)
    (send tab-panel set-selection 0)
    (goto-home-page tab-panel)
    (send tab-panel append "Introduction")
    (init-new-tab tab-panel 1)
    (send tab-panel set-selection 1)
    (goto-help-page tab-panel))

  ;(send tab-panel set-selection 0)

  ;; Select means copy for X11.
  (editor-set-x-selection-mode #t)

  ;(application-quit-handler quit)
  ;(application-about-handler about)


  ; Show the frame by calling its show method
  (send frame show #t))
