#lang racket/gui

(require net/sendurl)
(require "gopher.rkt")
(require "request.rkt")
(require "download.rkt")

(provide browser-text%
         browser-canvas%
         menu-item-snip%)

(struct browser-url
  (req
   selection-pos)  ;; position value or #f
  #:prefab)

(define (insert-menu-item text-widget dir-entity)
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
      [(#\h) "(HTML) "]
      [else  "(UNKN) "]))

  (define standard-style
    (send (send text-widget get-style-list) find-named-style "Standard"))
  (define link-style
    (send (send text-widget get-style-list) find-named-style "Link"))
  
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

  (define dir-entity (parse-dir-entity line))

  (cond
    ;; be permissive of blank lines
    [(not (non-empty-string? line))
     (send text-widget insert "\n")]
    ;; just skip/ignore end of transmission
    [(equal? (string-ref line 0) #\.)
     void]
    ;; display error line
    [(equal? (gopher-dir-entity-type dir-entity) #\3)
     (send text-widget insert (gopher-dir-entity-user-name dir-entity))
     (send text-widget insert "\n")]
    ;; insert informational lines as plain text
    [(equal? (gopher-dir-entity-type dir-entity) #\i)
     (send text-widget insert "       ")  ; indent information text to line up with menu items
     (send text-widget insert (gopher-dir-entity-user-name dir-entity))
     (send text-widget insert "\n")]
    [else
     (insert-menu-item text-widget dir-entity)
     (send text-widget insert "\n")]))

(define (request-updates-page? req)
  (cond
    [(equal? (request-type req) #\5) #f]
    [(equal? (request-type req) #\9) #f]
    [(equal? (request-type req) #\7) #f]
    [(equal? (request-type req) #\h) #f]
    [else #t]))

(define (download-only-type? type)
  (or (equal? type #\5)
      (equal? type #\9)))

(define (goto-gopher req page-text [initial-selection-pos #f])
  (eprintf "goto-gopher: ~a, ~a, ~a, ~a~n" (request-host req) (request-path/selector req) (request-type req) initial-selection-pos)

  (define resp (gopher-fetch (request-host req)
                             (request-path/selector req)
                             (request-type req)
                             (request-port req)))
  
  ;; default the item type to directory if not specified in request
  (define item-type (if (request-type req)
                        (request-type req)
                        #\1))

  ;; reset gopher-menu? boolean to default when loading a new page
  (when (request-updates-page? req)
    (set-field! gopher-menu? page-text #f))

  (send page-text begin-edit-sequence)
  (cond
    [(gopher-response-error? resp)
     (send page-text erase)
     (send page-text insert (port->string (gopher-response-data-port resp) #:close? #t))]
    [(equal? item-type #\1) ; directory
     (send page-text erase)
     (for ([line (in-lines (gopher-response-data-port resp))])
       (insert-directory-line page-text line))
     (close-input-port (gopher-response-data-port resp))
     (send page-text init-gopher-menu initial-selection-pos)]
    [(equal? item-type #\0) ; text
     (send page-text erase)
     ;; insert one line at a time to handle end of line conversion
     #;(for ([line (in-lines (gopher-response-data-port resp))])
       (send page-text insert line)
       (send page-text insert "\n"))
     ;; this isn't ideal but is still a lot faster than inserting one line at a time
     ;; (text% treats #\return as a newline so DOS formatted files have extra newlines)
     (send page-text insert (string-replace
                             (port->string (gopher-response-data-port resp))
                             "\r\n"
                             "\n"))
     (close-input-port (gopher-response-data-port resp))
     (send page-text set-position 0)]
    [(equal? item-type #\I) ; image
     (define img (make-object image-snip%
                              (gopher-response-data-port resp)
                              'unknown))
     (close-input-port (gopher-response-data-port resp))
     (send page-text erase)
     (send page-text insert img)
     (send page-text set-position 0)]
    [(equal? item-type #\g) ; gif
     (define img (make-object image-snip%
                              (gopher-response-data-port resp)
                              'gif))
     (close-input-port (gopher-response-data-port resp))
     (send page-text erase)
     (send page-text insert img)
     (send page-text set-position 0)]
    [(download-only-type? item-type) ; binary
     ;; this works around an issue in my server that I need to fix. it is automatically
     ;; closing sockets after ten seconds, causing the socket to timeout before the read can
     ;; complete due to the save file dialog. change this once I fix that.
     (define data (port->bytes (gopher-response-data-port resp) #:close? #t))
     (define path (put-file "Save file as..."))
     (eprintf "saving binary file to ~a~n" path)
     (with-output-to-file path
       (lambda () (write-bytes data)))]
    [else (void)])
  (send page-text end-edit-sequence))

(define (save-gopher-to-file req)
  (eprintf "save-gopher-to-file: ~a, ~a, ~a~n" (request-host req) (request-path/selector req) (request-type req))
  ;; get path from user
  (define path (put-file "Save file as..."))
  (eprintf "saving binary file to ~a~n" path)

  (when path
    (define resp (gopher-fetch (request-host req)
                               (request-path/selector req)
                               (request-type req)
                               (request-port req)))
    (track-download (current-thread) (gopher-response-data-port resp) path)
    (with-output-to-file path #:exists 'truncate
      (lambda ()
        (copy-port (gopher-response-data-port resp) (current-output-port))
        (mark-download-complete (current-thread))))))

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
                [thread-custodian #f]
                [current-url #f]
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
             get-canvas
             change-style
             find-first-snip
             find-snip
             in-edit-sequence?
             end-edit-sequence
             get-start-position get-end-position hide-caret)

    ;; copied from Framework's text:hide-caret/selection-mixin
    (define/augment (after-set-position)
      (hide-caret (= (get-start-position) (get-end-position)))
      (inner (void) after-set-position))

    (define/override (can-do-edit-operation? op [recursive? #t])
      (cond
        [(eq? op 'paste) #f]
        [else
         (super can-do-edit-operation? op recursive?)]))
    
    (define/private (push-history url)
      (set! history (cons url history)))

    (define/private (pop-history)
      (if (not (empty? history))
        (let ([top (car history)])
          (set! history (cdr history))
          top)
        '()))

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

    (define/private (load-page req [initial-selection-pos #f])
      ;; this will shutdown the previous custodian on every page load.
      ;; seems wasteful not to re-use the custodian if we aren't actually interrupting
      ;; the previous thread's work.
      (when (custodian? thread-custodian)
        (custodian-shutdown-all thread-custodian)
        ;; without this the editor gets stuck in no refresh mode
        (when (in-edit-sequence?)
          (end-edit-sequence)))
      
      (set! thread-custodian (make-custodian))
      (parameterize ([current-custodian thread-custodian])
        (thread (thunk
                 (goto-gopher req this initial-selection-pos)
                 (set! current-url (browser-url req initial-selection-pos))))))

    (define/public (go req)
      (when (request-updates-page? req)
        ;; add current page to history
        (when current-url
          (if selection
              ;; also save the position of the selection that we are following so we can return to it
              (push-history (struct-copy browser-url current-url [selection-pos (get-snip-position selection)]))
              (push-history current-url)))
        ;; set current-url to false while loading
        (set! current-url #f)        
        (send (get-canvas) update-address req))
      
      
      (cond
        [(download-only-type? (request-type req)) ; open save file dialog
         (thread (thunk
                  (save-gopher-to-file req)))]
        [(equal? (request-type req) #\7) ; gopher index search
         ;; prompt user for query string
         (define query-string (get-text-from-user "Query" "search string"))
         (define query-request (request (request-protocol req)
                                        (request-host req)
                                        (request-port req)
                                        (string-append (request-path/selector req) "\t" query-string)
                                        #\1))
         (when current-url
           (if selection
               ;; also save the position of the selection that we are following so we can return to it
               (push-history (struct-copy browser-url current-url [selection-pos (get-snip-position selection)]))
               (push-history current-url history)))
         (set! current-url #f)
         (send (get-canvas) update-address query-request)
         (load-page query-request)]
        [(equal? (request-type req) #\h)
         ; html, open in external browser
         (eprintf "opening ~a in browser~n" (gopher-url-request->url req))
         (send-url (gopher-url-request->url req) #t)]
        [else
         (load-page req)]))

    (define/public (go-back)
      (unless (empty? history)
        (define prev-url (pop-history))
        (set! current-url #f)
        (define req (browser-url-req prev-url))
        (send (get-canvas) update-address req)
        (load-page req (browser-url-selection-pos prev-url))))

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

    (define/public (load-restore-data list-of-data)
      (when (and (list? list-of-data)
                 (= (length list-of-data) 2))
        (set! history (cadr list-of-data))
        (when (browser-url? (car list-of-data))
          (go (browser-url-req (car list-of-data))))))
    
    ;; returns data necessary to restore this widget's state
    (define/public (get-restore-data)
      (list current-url history))

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
    (init-field [address-text-field #f]
                [status-bar #f]
                [tabpanel #f])
    (inherit get-editor)

    (define/private (set-tab-label label-text)
      (when tabpanel
        (define index (send tabpanel get-selection))
        (send tabpanel set-item-label index label-text)))

    (define/private (set-address-field address-text)
      (when address-text-field
        (send address-text-field set-value address-text)))

    ;; takes a request struct and updates UI elements
    (define/public (update-address req)
      (set-tab-label (string-append (request-host req) (request-path/selector req)))
      (set-address-field (request->url req)))
    
    (define/public (load-restore-data list-of-data)
      (define editor (get-editor))
      (when editor
        (send editor load-restore-data list-of-data)))
    
    ;; eventually this may need to handle multiple types of editors, but for now assume browser-text%
    (define/public (get-restore-data)
      (define editor (get-editor))
      (if editor
          (send editor get-restore-data)
          '()))
    ))

(define menu-item-snip%
  (class string-snip%
    (init-field [dir-entity #f])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    ;(set-flags (cons 'handles-events (get-flags)))

    ;; use clickbacks instead
    #;(define/override (on-event dc x y editorx editory e)
      (when (send e button-down? 'left)
        (follow-link)))
    ))
