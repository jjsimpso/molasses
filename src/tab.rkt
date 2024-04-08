#lang racket/gui

(require racket/serialize)

(require "widgets.rkt")
(require "config.rkt")
(require "request.rkt")

(provide init-new-tab
         active-page-canvas
         tab-panel-callback
         goto-home-page
         goto-help-page
         next-tab
         prev-tab
         new-tab
         delete-tab
         update-tab-order
         open-help-tab
         find-tp-address-field
         save-tabs
         load-tabs)

;; holds the ui component and data for a tab
(struct tab-info
  (id
   index
   contents)
  #:prefab
  #:mutable)

;; list of tab-info structs
(define tab-list '())

(define (print-tab-list l)
  (with-output-to-string
    (thunk
     (for ([tab (in-list l)])
       (printf "tab id:~a, index:~a~n" (tab-info-id tab) (tab-info-index tab))))))

(define tab-id-counter 0)
(define (new-tab-id)
  (set! tab-id-counter (add1 tab-id-counter))
  tab-id-counter)

;; returns a tab-info struct from the global tab-list or #f
(define (find-tab-at-index index)
  (for/first ([tab (in-list tab-list)]
              #:when (= index (tab-info-index tab)))
    tab))

;; returns a tab-info struct from the global tab-list or #f
(define (find-tab-with-id id)
  (for/first ([tab (in-list tab-list)]
              #:when (= id (tab-info-id tab)))
    tab))

(define (init-new-tab tp index)
  (printf "Init tab selection ~a~n" index)
  (send tp change-children
        (lambda (c*) '()))

  ;; callback called when the browser-canvas loads a new page
  ;; update the tab label for the canvas's tab and update the address text field in the tab
  (define (update-address id req)
    (define tab (find-tab-with-id id))
    ;; set the label for the tab
    (send tp set-item-label (tab-info-index tab) (string-append (request-host req) (request-path/selector req)))
    ;; set the address text field
    (send address-field set-value (request->url req))
    ;; disable/enable the back button based on existence of history
    (if (empty? (send page-canvas get-history))
        (send back-button enable #f)
        (send back-button enable #t)))

  ;; callback called when the browser-canvas needs to update a status message
  ;; update the text of the message% widget on the status bar. we only have
  ;; one global status bar, so the tab id parameter isn't used
  (define (update-status id text)
    (send status-msg set-label text))

  ;; gotta replace this at some point
  ;; gets the status-msg message% widget from status-bar
  (define status-msg
    (let ([frame (send tp get-top-level-window)])
      ;; only message% widget in the frame is the status-msg in the status bar
      (when frame
        (define children (send frame get-children))
        (for/first ([child (in-list children)]
                    #:when (is-a? child horizontal-pane%))
          (define grandchildren (send child get-children))
          (for/first ([grandchild (in-list grandchildren)]
                      #:when (is-a? grandchild message%))
            grandchild)))))
  
  ;; generate unique ID for the tab
  (define tab-id (new-tab-id))
  
  (define tab-contents
    (new vertical-panel% (parent tp)
         (alignment '(left top))
         (stretchable-width #t)
         (stretchable-height #t)
         (horiz-margin 0)))
    
  (define address-pane
    (new horizontal-pane% (parent tab-contents)
         (alignment '(left center))
         (stretchable-width #t)
         (stretchable-height #f)
         (horiz-margin 10)))

  (define back-button
    (new button% (parent address-pane)
         (label "\u2190") ; Back arrow
         (enabled #f)
         (horiz-margin 0)
         (callback
          (lambda (item event)
            (send page-canvas go-back)
            ;; set the focus back to the canvas
            (send page-canvas focus)))))
  
  #;(define forward-button
    (new button% (parent address-pane)
         (label "\u2192") ; Forward arrow
         (enabled #f)
         (horiz-margin 0)
         #;(callback (λ _ (go-forward)))))

  (define cancel-button
    (new button% (parent address-pane)
         (label "\u2715") ; cross mark
         (enabled #t)
         (horiz-margin 0)
         (callback
          (lambda (item event)
            (send page-canvas cancel-request)
            ;; set the focus back to the canvas
            (send page-canvas focus)))))

  (define address-field
    (new address-field% (parent address-pane)
         (label "")
         (init-value "")
         (style '(single))
         (callback
          (lambda (item event)
            (when (equal? (send event get-event-type)
                          'text-field-enter)
              (send page-canvas go (url->request (send item get-value)))
              (send page-canvas focus))))))

  (define page-canvas
    (new browser-canvas% (parent tab-contents)
         (tab-id tab-id)
         (default-bg-color canvas-bg-color)
         (update-status-cb update-status)
         (update-address-cb update-address)
         (smooth-scrolling #t)
         (wheel-step 10)))

  (init-styles (send page-canvas get-style-list))
  ;; init to gopher/gemini styles
  (send page-canvas set-default-style "Standard")

  #;(send* page-canvas
    (force-display-focus #t)
    (lazy-refresh #t))

  ;; set focus to address field when creating a new tab
  (send address-field focus)
  
  (set! tab-list
        (cons (tab-info tab-id index tab-contents)
              tab-list)))

(define (active-page-canvas tp)
  (find-tab-canvas (send tp get-selection)))

(define (goto-home-page tp)
  (define page-canvas (active-page-canvas tp))
  (send page-canvas go (url->request home-page-url))
  (send page-canvas focus))

(define (goto-help-page tp)
  (define page-canvas (active-page-canvas tp))
  (send page-canvas erase)
  (send page-canvas append-string #<<END
This is Molasses, your browser for the Slow Internet. If you think of
the Internet as an information super highway, then think of the Slow
Internet as a country road. You can use Molasses to return to a
simpler time. Currently Molasses supports two protocols: Gopher and
Gemini.  Gopher is an authentically old protocol while Gemini is a new
protocol with an old soul.

Molasses is a tabbed browser, so open as many tabs as you want! When
exiting, any open tabs are saved and will be restored the next time
you open the application.


GOPHER
------

Gopher is Molasses's default protocol. If an address is entered
without a protocol then it is assumed to be a Gopher address. 

Gopher menu navigation is based on Lynx and consists of the following
commands:

 * Up and Down arrows move the selection between menu items.
 * Right Arrow or Return follows the currently selected menu item 
   link. Left mouse click also follows links/menu items.
 * Left Arrow goes back to the previous page. There is also a back
   button on the Toolbar.
 * Page Up/Down scrolls the page without changing the selection, as
   does the mouse scroll wheel.
 * In text files, as opposed to Gopher menus, Up and Down arrows 
   scroll the page line-by-line.


GEMINI
------

Gemini navigation is entirely mouse-based. So using it is pretty much
the same as using a typical web browser.


GLOBAL SHORTCUT KEYS
--------------------
CTRL+L     : Select address text
CTRL+T     : New Tab
CTRL+PGUP  : Next Tab
CTRL+PGDWN : Next Tab


DOWNLOADING FILES
-----------------
If the type of a Gopher menu item or Gemini link is not supported
natively by Molasses, the file can still be downloaded for local
viewing. Progress of active and completed downloads can be viewed by
clicking on "Downloads" in the bottom right hand corner of the UI. A
pop-up menu will show all downloaded files and their progress. 
Selecting an item from this list will either cancel the download or,
in the case of completed downloads, remove the entry from the download
list.
END
        )
  (send page-canvas scroll-to 0)
  (send page-canvas focus))

(define (next-tab tp)
  (define num-tabs (send tp get-number))
  (define new-tab (add1 (send tp get-selection)))
  (unless (>= new-tab num-tabs)
    (send tp set-selection new-tab)
    (change-tab tp new-tab)))

(define (prev-tab tp)
  (define new-tab (sub1 (send tp get-selection)))
  (unless (< new-tab 0)
    (send tp set-selection new-tab)
    (change-tab tp new-tab)))

(define (open-help-tab tp)
  ;; get index of the new help tab
  (define help-tab-index (send tp get-number))
  (send tp append "Introduction")
  (send tp set-selection help-tab-index)
  (init-new-tab tp help-tab-index)
  (change-tab tp help-tab-index)
  (goto-help-page tp))

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

  (define (make-header-style name size)
    ;; Each style created with this procedure copies "Standard" style
    ;; and creates a new style by name 'name' and with the size 'size'.
    (send (send style-list new-named-style name standard)
          set-delta (send* (make-object style-delta%)
                      (copy standard-delta)
                      (set-delta 'change-weight 'bold)
                      (set-delta 'change-size size))))
  
  (make-color-style "Link" link-color)
  (make-color-style "Link Highlight" link-highlight-color)
  (make-header-style "Header1" 24)
  (make-header-style "Header2" 18)
  (make-header-style "Header3" 14)

  ;; create default html style
  (define html-standard (send style-list new-named-style "Html Standard" standard))
  (define html-standard-delta (make-object style-delta%))
  (send* html-standard-delta
    (set-family 'roman)
    (set-delta 'change-size 11)
    (set-delta-foreground html-text-fg-color)
    (set-delta-background html-text-bg-color))
  (send html-standard set-delta html-standard-delta)
)

(define (find-tp-address-field tp)
  (define tab (find-tab-at-index (send tp get-selection)))
  (when tab
    (define children (send (tab-info-contents tab) get-children))
    ;(eprintf "tab children: ~a~n" children)
    (for/first ([child (in-list children)]
                #:when (is-a? child horizontal-pane%))
      (for/first ([grandchild (in-list (send child get-children))]
                  #:when (is-a? grandchild text-field%))
        grandchild))))

;; returns the tab at index's canvas widget or #f
(define (find-tab-canvas index)
  (define tab (find-tab-at-index index))
  (when tab
    (define children (send (tab-info-contents tab) get-children))
    ;(eprintf "tab children: ~a~n" children)
    (for/first ([child (in-list children)]
                #:when (is-a? child browser-canvas%))
      child)))

(define (change-tab tp tab-index)
  ;(eprintf "changing to tab ~a~n" tab-index)
  (fill-tab-content tp)
  (define tab-canvas (find-tab-canvas tab-index))
  (when tab-canvas
    ; this will set the status bar's message to the current status of the new tab
    (send tab-canvas update-status)
    (send tab-canvas focus))
  void)

(define (fill-tab-content tp)
  (define current-tab-index (send tp get-selection))
  (send tp change-children
        (lambda (c*)
          ;; return contents of the tab as a list
          (list (tab-info-contents (find-tab-at-index current-tab-index))))))

(define (tab-panel-callback item event)
  (define tab-index (send item get-selection))
  (change-tab item tab-index))

(define (new-tab tp)
  (define new-index (send tp get-number))
  (send tp append "New")
  (init-new-tab tp new-index)
  (send tp set-selection new-index)
  (change-tab tp new-index)
  ;; set the focus to the address field
  (send (find-tp-address-field tp) focus))

(define (delete-tab tp tab-index)
  (define num-tabs  (send tp get-number))
  ;; can't delete the only tab
  (unless (= num-tabs 1)
    ;; remove the deleted tab from our global list of tabs
    (set! tab-list
          (remove (tab-info null tab-index null)
                  tab-list
                  (lambda (a b)
                    (= (tab-info-index a)
                       (tab-info-index b)))))
    ;; for every tab with an index > than the removed tab, reduce its index by one
    (for ([tab (in-list tab-list)])
      (when (> (tab-info-index tab) tab-index)
        (set-tab-info-index! tab (sub1 (tab-info-index tab)))))
    ;; delete the tab from the panel
    (send tp delete tab-index)
    ;; change to the new tab in the same location unless we deleted the last tab,
    ;; then change to the new last tab at tab-index - 1.
    (if (= tab-index (sub1 num-tabs))
        (begin
          (send tp set-selection (sub1 tab-index))
          (change-tab tp (sub1 tab-index)))
        (begin
          (send tp set-selection tab-index)
          (change-tab tp tab-index)))))

(define (update-tab-order tp former-indices)
  ;(eprintf "former-indices: ~a~n" former-indices)
  ;(eprintf "old tab list:~n~a" (print-tab-list tab-list))
  ;; put the list of tabs in the reverse order of their new indices
  ;; tab-list is basically a stack with the rightmost tab on the top
  (define new-tab-list
    (foldl (lambda (old-index new-list)
             (cons (find-tab-at-index old-index) new-list))
           '()
           former-indices))
  ;(eprintf "between tab list:~n~a" (print-tab-list new-tab-list))
  ;; update the tab info structs with the index to match their new position
  (for ([i (in-range (sub1 (length new-tab-list)) -1 -1)]
        [tab (in-list new-tab-list)])
    (set-tab-info-index! tab i))
  ;; update the global tab-list
  (set! tab-list new-tab-list)
  ;(eprintf "new tab list:~n~a" (print-tab-list tab-list))
  ;; the moved tab should be the new active tab
  (define new-selection (index-of former-indices (send tp get-selection)))
  (send tp set-selection new-selection)
  (change-tab tp new-selection))

(define (tab-info->save-data tab)
  (define index (tab-info-index tab))
  (define canvas (find-tab-canvas index))
  ;(eprintf "Saving tab ~a: ~a~n" index (send canvas get-restore-data))
  (serialize (send canvas get-restore-data)))

(define (save-tabs tp)
  ;(eprintf "Saving tabs~n")
  (define num-tabs (send tp get-number))
  (define tabs
    (let loop ([index (sub1 num-tabs)]
               [tabs '()])
      (cond
        [(< index 0)
         tabs]
        [else
         (loop (sub1 index)
               (cons (tab-info->save-data (find-tab-at-index index))
                     tabs))])))
  (put-preferences '(tabs) (list tabs) #f tabs-file))

(define (load-tabs tp)
  (define tabs-pref
    (get-preference 'tabs
                    (lambda () #f)
                    'timestamp
                    tabs-file))
  ;(printf "tab pref: ~a~n" tabs-pref)
  (if tabs-pref
      (let ([num-tabs (length tabs-pref)])
        ;(eprintf "Restoring ~a tabs~n" num-tabs)
        (for ([tab (in-list tabs-pref)]
              [index (in-naturals)])
          (eprintf "Restoring tab ~a: ~a~n" index (deserialize tab))
          (send tp append "New")
          (send tp set-selection index)
          (init-new-tab tp index)
          (define canvas (find-tab-canvas index))
          (send canvas load-restore-data (deserialize tab))
          (send canvas focus)))
      #f))
