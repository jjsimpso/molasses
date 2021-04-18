#lang racket/gui

(require net/url-string)
(require "gopher.rkt")
(require "request.rkt")

(provide browser-text%
         browser-canvas%
         menu-item-snip%)

(struct browser-url
  (req
   selection-pos)  ;; position value or #f
  #:prefab)

(define (insert-menu-item text-widget line)
  (define (gopher-menu-type-text type)
    (case type
      [(#\0) "(TEXT) "]
      [(#\1) " (DIR) "]
      [(#\3) " (ERR) "]
      [(#\5 #\9) " (BIN) "]
      [(#\g) " (GIF) "]
      [(#\I) " (IMG) "]
      [(#\7) "(SRCH) "]
      [(#\8) " (TEL) "]
      [else  "(UNKN) "]))

  (define standard-style
    (send (send text-widget get-style-list) find-named-style "Standard"))
  (define link-style
    (send (send text-widget get-style-list) find-named-style "Link"))
  
  (define dir-entity (parse-dir-entity line))
  (define type-snip (new string-snip%))
  (define link-snip (new menu-item-snip% (dir-entity dir-entity)))
  
  (define type-text (gopher-menu-type-text (gopher-dir-entity-type dir-entity)))
  (define display-text (gopher-dir-entity-user-name dir-entity))

  ;; insert text for type indication
  (send type-snip set-style standard-style)
  (send type-snip insert type-text (string-length type-text))
  (send text-widget insert type-snip)
  
  (define link-start-pos (send text-widget last-position))
  (send link-snip set-style link-style)
  (send link-snip insert display-text (string-length display-text)) ;(send link-snip get-count))
  (send text-widget insert link-snip)
  (send text-widget change-style standard-style)
  ;; add clickback to link region
  (send text-widget set-clickback
        link-start-pos
        (send text-widget last-position)
        selection-clickback-handler)
)

(define (insert-directory-line text-widget line)
  ;(eprintf "insert-directory-line: ~a~n" line)
  (cond
    [(not (non-empty-string? line))
     (send text-widget insert "\n")]
    ;; display error line
    [(equal? (string-ref line 0) #\3)
     (define text (car (string-split (substring line 1) "\t" #:trim? #f)))
     (send text-widget insert text)
     (send text-widget insert "\n")]
    ;; insert informational lines as plain text
    [(equal? (string-ref line 0) #\i)
     (define text (car (string-split (substring line 1) "\t" #:trim? #f)))
     (send text-widget insert "       ")  ; indent information text to line up with menu items
     (send text-widget insert text)
     (send text-widget insert "\n")]
    ;; just skip/ignore end of transmission
    [(equal? (string-ref line 0) #\.)
     void]
    [else
     (insert-menu-item text-widget line)
     (send text-widget insert "\n")]))

(define (request-updates-page? req)
  (cond
    [(equal? (request-type req) #\5) #f]
    [(equal? (request-type req) #\9) #f]
    [(equal? (request-type req) #\7) #f]
    [else #t]))

;; if type isn't set, the gopher item type is determined from the URL
(define (goto-gopher req page-text [initial-selection-pos #f])
  (eprintf "goto-gopher: ~a, ~a, ~a, ~a~n" (request-host req) (request-path/selector req) (request-type req) initial-selection-pos)

  (define resp (gopher-fetch (request-host req)
                             (request-path/selector req)
                             (request-type req)
                             (request-port req)))
  
  ;; default the item type to directory
  (define item-type (if (gopher-response-item-type resp)
                        (gopher-response-item-type resp)
                        #\1))

  ;; reset gopher-menu? boolean to default when loading a new page
  (when (request-updates-page? req)
    (set-field! gopher-menu? page-text #f))
  
  (cond
    [(gopher-response-error? resp)
     (send page-text erase)
     (send page-text insert (gopher-response-data resp))]
    [(equal? item-type #\1) ; directory
     (send page-text erase)
     (for ([line (in-lines (open-input-bytes (gopher-response-data resp)))])
       (insert-directory-line page-text line))
     (send page-text init-gopher-menu initial-selection-pos)]
    [(equal? item-type #\0) ; text
     (send page-text erase)
     ;; insert one line at a time to handle end of line conversion
     (for ([line (in-lines (open-input-bytes (gopher-response-data resp)))])
       (send page-text insert line)
       (send page-text insert "\n"))
     (send page-text set-position 0)]
    [(equal? item-type #\I) ; image
     (define img (make-object image-snip%
                              (open-input-bytes (gopher-response-data resp))
                              'unknown))
     (send page-text erase)
     (send page-text insert img)
     (send page-text set-position 0)]
    [(equal? item-type #\g) ; gif
     (define img (make-object image-snip%
                              (open-input-bytes (gopher-response-data resp))
                              'gif))
     (send page-text erase)
     (send page-text insert img)
     (send page-text set-position 0)]
    [(or (equal? item-type #\5) (equal? item-type #\9)) ; binary
     (define path (put-file "Save file as..."))
     (eprintf "saving binary file to ~a~n" path)
     (with-output-to-file path
       (lambda () (write-bytes (gopher-response-data resp))))]
    [else (void)]))

(define (selection-clickback-handler text-widget start end)
  (define snip (send text-widget find-snip start 'after))
  (eprintf "clickback: start=~a, snip=~a~n" start snip)
  (when (and snip (is-a? snip menu-item-snip%))
    (set-field! selection text-widget snip)
    (define dir-entity (get-field dir-entity snip))
    (send text-widget go (dir-entity->request dir-entity))))

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
                [gopher-menu? #f]
                [address-text-field #f]
                [current-url (browser-url #f #f)]
                [history '()]) ; list of browser-url structs
    (inherit get-snip-position
             set-position
             set-position-bias-scroll
             move-position
             scroll-to-position
             line-start-position
             position-line
             last-position
             get-visible-position-range
             get-visible-line-range
             get-style-list
             change-style
             find-first-snip
             find-snip)

    ;; starting from snip, find the next menu snip in between the the lines start and end
    ;; if snip is already in the region, just return it
    (define/private (find-next-menu-snip-in-region snip start end)
      (define current-line (position-line (get-snip-position snip)))
      (cond
        [(> current-line end) snip]
        [(< current-line start)
         (define next-snip (find-next-menu-snip snip))
         (if next-snip
             (find-next-menu-snip-in-region next-snip start end)
             snip)]
        [(and (>= current-line start)
              (<= current-line end))
         snip]))

    ;; starting from snip, find the previous menu snip in between the the lines start and end
    ;; if snip is already in the region, just return it
    (define/private (find-prev-menu-snip-in-region snip start end)
      (define current-line (position-line (get-snip-position snip)))
      (cond
        [(< current-line start) snip]
        [(> current-line end)
         (define prev-snip (find-prev-menu-snip snip))
         (if prev-snip
             (find-prev-menu-snip-in-region prev-snip start end)
             snip)]
        [(and (>= current-line start)
              (<= current-line end))
         snip]))
    
    (define/public (find-first-menu-snip)
      (define first-snip (find-first-snip))
      (if (is-a? first-snip menu-item-snip%)
          first-snip
          (find-next-menu-snip first-snip)))

    (define/public (init-gopher-menu [initial-selection-pos #f])
      (set! gopher-menu? #t)
      (if initial-selection-pos
          (set! selection (find-snip initial-selection-pos 'after)) 
          (set! selection (find-first-menu-snip)))
      (when selection
        (define new-style (send (get-style-list) find-named-style "Link Highlight"))
        (define pos (get-snip-position selection))
        (set-position pos 'same #f #t 'default)
        (change-style new-style
                      pos
                      (+ pos (send selection get-count)))
        (if initial-selection-pos
            ;; make the selection visible
            (scroll-to-position initial-selection-pos)
            ;; scroll to the beginning
            (scroll-to-position 0))))

    (define/public (go req)
      (when (request-updates-page? req)
        ;; add current page to history
        (if selection
            ;; also save the position of the selection that we are following so we can return to it
            (set! history (cons (struct-copy browser-url current-url [selection-pos (get-snip-position selection)])
                                history))
            (set! history (cons current-url history)))
        (set! current-url (browser-url req #f))

        ;; set the address field's value string to the new url, adding gopher type if necessary
        (send address-text-field set-value (request->url req)))
      
      (cond
        [(equal? (request-type req) #\7) ; gopher index search
         ;; prompt user for query string
         (define query-string (get-text-from-user "Query" "search string"))
         (define query-request (request (request-protocol req)
                                        (request-host req)
                                        (request-port req)
                                        (string-append (request-path/selector req) "\t" query-string)
                                        #\1))
         (if selection
             ;; also save the position of the selection that we are following so we can return to it
             (set! history (cons (struct-copy browser-url current-url [selection-pos (get-snip-position selection)])
                                 history))
             (set! history (cons current-url history)))
         (set! current-url (browser-url query-request #f))
         (send address-text-field set-value (request->url query-request))
         (goto-gopher query-request this)]
        [else
         (goto-gopher req this)]))

    (define/public (go-back)
      (unless (empty? history)
        (set! current-url (car history))
        (send address-text-field set-value (request->url (browser-url-req current-url)))
        (goto-gopher (browser-url-req current-url)
                     this
                     (browser-url-selection-pos current-url))
        (set! history (cdr history))))

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
      (if gopher-menu?
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
                  (define start (box 0))
                  (define end (box 0))
                  (get-visible-line-range start end #f)
                  (set-position pos 'same #f #t 'default)
                  (when (>= (position-line pos) (unbox end))
                    ;; scroll down to show the next page
                    (define new-start (line-start-position (unbox end)))
                    (eprintf "scrolling to line ~a-~a~n" (unbox end) (+ (unbox end) (- (unbox end) (unbox start))))
                    (scroll-to-position new-start
                                        #f
                                        (max pos
                                             (line-start-position (sub1 (+ (unbox end)
                                                                           (- (unbox end) (unbox start))))))
                                        'end)))]
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
               (define start (box 0))
               (define end (box 0))
               (get-visible-line-range start end #f)
               (set-position pos 'same #f #t 'default)
               (when (< (position-line pos) (unbox start))
                 ;; scroll up to show the previous page
                 (define new-end (line-start-position (sub1 (unbox start))))
                 (eprintf "scrolling to line ~a-~a~n" (max 0 (- (unbox start) (- (unbox end) (unbox start)))) (sub1 (unbox start)))
                 (scroll-to-position (min pos
                                          (line-start-position (max 0  ; start position cannot be negative
                                                                    (- (unbox start)
                                                                       (- (unbox end) (unbox start))))))
                                     #f
                                     new-end
                                     'start)))]
            [(left)
             (go-back)]
            [(right #\return)
             (when selection
               (define dir-entity (get-field dir-entity selection))
               (go (dir-entity->request dir-entity)))]
            [(next)
             (define start (box 0))
             (define end (box 0))
             (get-visible-line-range start end #f)
             (define new-start (add1 (unbox end)))
             (define new-end (+ (unbox end)
                                (- (unbox end) (unbox start))))
             (scroll-to-position (line-start-position new-start)
                                 #f
                                 (line-start-position new-end)
                                 'end)
             (when selection
               (define item (find-next-menu-snip-in-region selection new-start new-end))
               (when (and item (not (eq? item selection)))
                 (define pos (get-snip-position item))
                 (change-highlight selection item)
                 (set! selection item)
                 (set-position pos 'same #f #t 'default)))]
            [(prior)
             (define start (box 0))
             (define end (box 0))
             (get-visible-line-range start end #f)
             (define new-start (max 0
                                    (- (unbox start)
                                       (- (unbox end) (unbox start)))))
             (define new-end (max 0 (sub1 (unbox start))))
             (scroll-to-position (line-start-position new-start)
                                 #f
                                 (line-start-position new-end)
                                 'start)
             (when selection
               (define item (find-prev-menu-snip-in-region selection new-start new-end))
               (when (and item (not (eq? item selection)))
                 (define pos (get-snip-position item))
                 (change-highlight selection item)
                 (set! selection item)
                 (set-position pos 'same #f #t 'default)))]
            [else
             ;(define key-code (send event get-key-code))
             ;(eprintf "browser-text on-local-char: unhandled key ~a~n" key-code)
             (void)])
          (case (send event get-key-code)
            [(left)
             (go-back)]
            [(right)
             (void)]
            [(up)
             ;; scroll the screen up one line
             (define start (box 0))
             (define end (box 0))
             (get-visible-line-range start end #f)
             (define new-line (max 0 (sub1 (unbox start))))
             ;(eprintf "range (~a,~a): new line = ~a~n" (unbox start) (unbox end) new-line)
             (set-position (line-start-position new-line))]
            [(down)
             ;; scroll the screen one line
             (define start (box 0))
             (define end (box 0))
             (get-visible-line-range start end #f)
             ;; setting the position to the last line will scroll the screen by one
             ;; don't have to actually set the position to the next visible line
             (define new-line (min (position-line (last-position))
                                   (unbox end)))
             ;(eprintf "range (~a,~a): new line = ~a~n" (unbox start) (unbox end) new-line)
             (set-position (line-start-position new-line))]
            [else
             (super on-local-char event)])))
    ))

(define browser-canvas%
  (class editor-canvas% (super-new)
    (init-field [selection #f])
    (inherit get-editor)
    ))

(define menu-item-snip%
  (class string-snip%
    (init-field [dir-entity #f])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    ;(set-flags (cons 'handles-events (get-flags)))

    (define/public (follow-link)
      (eprintf "follow-link: ~a~n" url))

    ;; use clickbacks instead
    #;(define/override (on-event dc x y editorx editory e)
      (when (send e button-down? 'left)
        (follow-link)))
    ))
