#lang racket/gui

(require "config.rkt")

(provide add-bookmark
         populate-bookmark-menu
         load-bookmarks
         save-bookmarks
         open-bookmark-editor)

(define bookmark-list '())

;; goto-bookmark is a function which takes a url text string
(define (add-bookmark-to-menu url label menu goto-bookmark)
  (new menu-item%
       [parent menu]
       [label label]
       [callback
        (lambda (item ctrl-event)
          (goto-bookmark url))]))

(define (add-bookmark url label menu goto-bookmark)
  #;(eprintf "adding bookmark ~a,~a~n" label url)
  (set! bookmark-list (append bookmark-list (list (cons label url))))
  ;(display bookmark-list)
  (add-bookmark-to-menu url label menu goto-bookmark)
  (save-bookmarks))

(define (populate-bookmark-menu menu goto-bookmark)
  (for ([bookmark (in-list bookmark-list)])
    (add-bookmark-to-menu (cdr bookmark) (car bookmark) menu goto-bookmark)))

(define (load-bookmarks)
  (set! bookmark-list
        (get-preference 'bookmarks
                        (lambda () '())
                        'timestamp
                        bookmarks-file)))

(define (save-bookmarks)
  (put-preferences '(bookmarks) (list bookmark-list) #f bookmarks-file))

(define (open-bookmark-editor parent-frame menu-w-bookmarks goto-bookmark)
  (define bm-list bookmark-list)

  (define (delete-bookmark)
    (define selections (send lb get-selections))
    (when (not (empty? selections))
      (define selected-idx (car selections))
      (define selected-bookmark (list-ref bm-list selected-idx))
      (when selected-bookmark
        (printf "deleting bookmark ~a~n" selected-bookmark)
        (send lb delete selected-idx)
        (set! bm-list (remove selected-bookmark bm-list)))))

  (define (swap-bookmarks distance)
    (define selections (send lb get-selections))
    (when (not (empty? selections))
      (define selected-idx (car selections))
      (define swap-idx (+ selected-idx distance))
      (define max-idx (sub1 (send lb get-number)))
      (when (and (>= swap-idx 0) (<= swap-idx max-idx))
        (define selected-bookmark (list-ref bm-list selected-idx))
        (define swap-bookmark (list-ref bm-list swap-idx))
        (set! bm-list (list-set bm-list swap-idx selected-bookmark))
        (set! bm-list (list-set bm-list selected-idx swap-bookmark))
        (define selected-name (send lb get-string selected-idx))
        (define selected-url (send lb get-data selected-idx))
        (define swap-name (send lb get-string swap-idx))
        (define swap-url (send lb get-data swap-idx))
        (send lb set-string swap-idx selected-url 1)
        (send lb set-string swap-idx selected-name 0)
        (send lb set-data swap-idx selected-url)
        (send lb set-string selected-idx swap-url 1)
        (send lb set-string selected-idx swap-name 0)
        (send lb set-data selected-idx swap-url)
        (send lb select swap-idx))))

  (define be-frame
    (new
     (class frame% (super-new)
       (define/override (on-subwindow-char receiver event)
         (case (send event get-key-code)
           [(#\rubout) (delete-bookmark)]
           [else
            (super on-subwindow-char receiver event)])))
       [label "Bookmark Editor"]
       [parent parent-frame]
       [width (exact-truncate (* (send parent-frame get-width) 0.5))]
       [height (exact-truncate (* (send parent-frame get-height) 0.5))]
       [style '(no-system-menu)]))

  (define p (new vertical-pane% [parent be-frame]))

  (define lb
    (new list-box%
         [label ""]
         [parent p]
         [choices '()]
         [style '(single column-headers)]
         [columns '("Site Name" "URL")]
         [callback
          (lambda (item event)
            (define selections (send item get-selections))
            (when (not (empty? selections))
              (define selected-idx (car selections))
              (when (eq? (send event get-event-type) 'list-box)
                (define selected-bookmark (list-ref bm-list selected-idx))
                (printf "selected bookmark: ~a - ~a~n" (car selected-bookmark) (cdr selected-bookmark)))))]))

  (define button-pane
    (new horizontal-pane%
         [parent p]
         [alignment '(center center)]
         [stretchable-height #f]))

  (define up-button
    (new button%
         (label "Up")
         (parent button-pane)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (swap-bookmarks -1)))))

  (define down-button
    (new button%
         (label "Down")
         (parent button-pane)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (swap-bookmarks 1)))))

  (define delete-button
    (new button%
         (label "Delete Selected")
         (parent button-pane)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (delete-bookmark)))))

  (define cancel-button
    (new button%
         (label "Cancel")
         (parent button-pane)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (send be-frame show #f)))))
  
  (define save-button
    (new button%
         (label "Save Changes")
         (parent button-pane)
         (stretchable-width #f)
         (callback
          (lambda (item event)
            (set! bookmark-list bm-list)
            (save-bookmarks)
            ;; delete all bookmark menu items
            (for ([item (in-list (send menu-w-bookmarks get-items))]
                  #:when (is-a? item labelled-menu-item<%>))
              (define label (send item get-plain-label))
              (unless (or (equal? label "Bookmark Page")
                          (equal? label "Edit Bookmarks")
                          (equal? label "Home"))
                (send item delete)))

            ;; add the edited bookmark list to the menu
            (for ([bm (in-list bm-list)])
              (add-bookmark-to-menu (cdr bm) (car bm) menu-w-bookmarks goto-bookmark))
            
            (send be-frame show #f)))))

  (define url-list (map cdr bm-list))
  (send lb set (map car bm-list) url-list)
  ;; the only way to get the values for the second column is to store it as per-item data
  (for ([url (in-list url-list)]
        [i (in-naturals 0)])
    (send lb set-data i url))
  (define half-width (exact-truncate (* (send be-frame get-width) 0.5)))
  (send lb set-column-width 0 half-width 100 2000)
  (send lb set-column-width 1 half-width 100 2000)
  (send be-frame show #t)
  (send be-frame focus))
