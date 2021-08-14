#lang racket/gui

(require net/sendurl)
(require "request.rkt")
(require "gopher.rkt")
(require "gemini.rkt")
(require "download.rkt")

(provide browser-text%
         browser-canvas%
         menu-item-snip%
         address-field%)

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
  (set-field! gopher-menu? page-text #f)

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
    [(or (equal? item-type #\0) (equal? item-type #\h)) ; text or html(just display raw html for now)
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
     (send page-text erase)
     (send page-text insert img)
     (send page-text set-position 0)
     (close-input-port (gopher-response-data-port resp))]
    [(equal? item-type #\g) ; gif
     (define img (make-object image-snip%
                              (gopher-response-data-port resp)
                              'gif))
     (close-input-port (gopher-response-data-port resp))
     (send page-text erase)
     (send page-text insert img)
     (send page-text set-position 0)]
    [else
     (send page-text erase)
     (send page-text insert (format "Unsupported type ~a~n~n" item-type))
     (insert-menu-item page-text
                       (gopher-dir-entity #\9 "Download file" (request-path/selector req) (request-host req) (~a (request-port req))))
     (send page-text init-gopher-menu #f)
     (close-input-port (gopher-response-data-port resp))])
  (send page-text end-edit-sequence))

(define (save-gopher-to-file req)
  (eprintf "save-gopher-to-file: ~a, ~a, ~a~n" (request-host req) (request-path/selector req) (request-type req))
  ;; get path from user
  (define path (put-file "Save file as..." #f #f (file-name-from-path (string->path (request-path/selector req)))))
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

(define (save-gemini-to-file data-port remote-path)
  (eprintf "save-gemini-to-file~n")
  ;; get path from user
  (define path (put-file "Save file as..." #f #f (file-name-from-path (string->path remote-path))))
  (eprintf "saving binary file to ~a~n" path)

  (when path
    (track-download (current-thread) data-port path)
    (with-output-to-file path #:exists 'truncate
      (lambda ()
        (copy-port data-port (current-output-port))
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

; Links take the form "=>[<whitespace>]<URL>[<whitespace><USER-FRIENDLY LINK NAME>]<CR><LF>"
(define gemini-link-re #px"^=>\\s*(\\S*)\\s*(.*)")

; Headers start with "#", "##", or "###".
(define gemini-header-re #px"^#{1,3} .*")

(define gemini-pre-re #px"^```.*")

(define (insert-gemini-text text-widget data-port base-url)
  (define standard-style
    (send (send text-widget get-style-list) find-named-style "Standard"))
  (define link-style
    (send (send text-widget get-style-list) find-named-style "Link"))
  (define header1-style
    (send (send text-widget get-style-list) find-named-style "Header1"))
  (define header2-style
    (send (send text-widget get-style-list) find-named-style "Header2"))
  (define header3-style
    (send (send text-widget get-style-list) find-named-style "Header3"))
  
  ; We'll just display them as bold text
  (define (line->header line)  
    (define octothorpe-count
      (if (equal? (string-ref line 1) #\#)
          (if (equal? (string-ref line 2) #\#)
              3
              2)
          1))
    (define text-snip (new string-snip%))
    (send text-snip set-style
          (case octothorpe-count
            [(1) header1-style]
            [(2) header2-style]
            [(3) header3-style]))
    ;; skip the mandatory space character after the last '#'
    (define header-string (substring line (add1 octothorpe-count)))
    (send text-snip insert header-string (string-length header-string))
    text-snip)
  
  ; Plain text lines are anything else
  (define (line->text text)
    (define text-snip (new string-snip%))
    (send text-snip set-style standard-style)
    (send text-snip insert text (string-length text))
    text-snip)

  (define (make-link text url)
    (define link-snip (new gemini-link-snip% (url url)))
    (send link-snip set-style link-style)
    (send link-snip insert text (string-length text))
    link-snip)

  ;; returns 2 values: the url and user-friendly name, which could be equal
  (define (parse-link line)
    (define line-len (string-length line))
    ;; start parsing after the '=>'
    (let loop ([start-pos 2])
      (if (char-blank? (string-ref line start-pos))
          (loop (add1 start-pos))
          ;; found start of url
          (let loop2 ([index start-pos]
                      [url-pos start-pos])
            (cond
              [(= index line-len)
               ;; end of string, no user-friendly name
               (define url (substring line url-pos))
               (values url url)]
              [(char-blank? (string-ref line index))
               ;; start looking for start of user-friendly name
               (let loop3 ([index (add1 index)]
                           [url (substring line url-pos index)]
                           [name-pos (add1 index)])
                 (cond
                   [(= index line-len)
                    ;; end of string, end of user-friendly name
                    (values url (substring line name-pos))]
                   [(and (char-blank? (string-ref line index))
                         (= index name-pos))
                    ;; skip initial whitespace
                    (loop3 (add1 index) url (add1 index))]
                   [(char-blank? (string-ref line index))
                    ;; whitespace within the user-friendly name
                    (loop3 (add1 index) url name-pos)]
                   [else
                    (loop3 (add1 index) url name-pos)]))]
              [else
               (loop2 (add1 index) url-pos)])))))
    
  (define (replace-final-path-element path relative-path)
    (string-append (path->string (path-only path)) relative-path))
  
  (for ([line (in-lines data-port)])
    (match line
      [(regexp gemini-link-re)
       (define-values (link-url link-name) (parse-link line))
       (define link-start-pos (send text-widget last-position))
       (eprintf "link url=~a, name=~a~n" link-url link-name)
       (send text-widget insert (make-link link-name link-url))
       ;; add clickback to link region
       (send text-widget set-clickback
             link-start-pos
             (send text-widget last-position)
             (lambda (text-widget start end)
               (eprintf "following gemini link: ~a~n" link-url)
               (if (regexp-match #px"^(\\w+://).*" link-url)
                   (cond
                     [(string-prefix? link-url "gemini://")
                      (send text-widget go (url->request link-url))]
                     [(string-prefix? link-url "gopher://")
                      (send text-widget go (url->request link-url))]
                     [(or (string-prefix? link-url "http://")
                          (string-prefix? link-url "https://"))
                      (send-url link-url #t)])
                   ;; handle partial URLs
                   (let ([base-req (url->request base-url)])
                     (send text-widget go
                           (struct-copy request
                                        base-req
                                        [path/selector
                                         (if (equal? (string-ref link-url 0) #\/)
                                             link-url
                                             (replace-final-path-element (request-path/selector base-req) link-url))]))))))]

      [(regexp gemini-header-re)
       (send text-widget insert (line->header line))]
      [(regexp gemini-pre-re)
       ;; read preformatted text until we read another "```". technically should create
       ;; a style for this that has a fixed width font, but our default font already
       ;; is fixed width.
       (let loop ([line (read-line data-port)])
         (unless (or (regexp-match gemini-pre-re line) (equal? line eof))
           (send text-widget insert (line->text line))
           (send text-widget insert "\n")
           (loop (read-line data-port))))]
      [_ (send text-widget insert (line->text line))])
    (send text-widget insert "\n")))

;; unlike goto-gopher, goto-gemini returns a request
;; the initial request can be forwaded to a new request for queries
;; 
(define (goto-gemini req page-text)
  (eprintf "goto-gemini: ~a, ~a~n" (request-host req) (request-path/selector req))

  (define (show-gemini-error msg)
    (send page-text begin-edit-sequence)
    (send page-text erase)
    (send page-text insert msg)
    (send page-text end-edit-sequence)
    (close-input-port (gemini-response-data-port resp)))
  
  (define resp (gemini-fetch (request-host req)
                             (request-path/selector req)
                             (request-port req)))

  (eprintf "goto-gemini: status=~a, meta=~a, from-url=~a~n" (gemini-response-status resp) (gemini-response-meta resp)
           (gemini-response-from-url resp))

  ;; the case needs to be in tail position so that it returns the correct value to load-page
  (case (gemini-response-status resp)
    [(10)
     (define query-string (get-text-from-user "Input required" (gemini-response-meta resp)))
     (define query-request (url->request (string-append (gemini-response-from-url resp) "?" query-string)))
     (close-input-port (gemini-response-data-port resp))
     (goto-gemini query-request page-text)]
    [(20 21)
     (let ([data-port (gemini-response-data-port resp)]
           [mimetype (gemini-response-meta resp)]
           [from-url (gemini-response-from-url resp)])
       (cond
         [(string-prefix? mimetype "text/gemini")
          (send page-text begin-edit-sequence)
          (send page-text erase)
          (insert-gemini-text page-text data-port from-url)
          (send page-text set-position 0)
          (send page-text end-edit-sequence)]
         [(string-prefix? mimetype "text/")
          (send page-text begin-edit-sequence)
          (send page-text erase)
          ;; this isn't ideal but is still a lot faster than inserting one line at a time
          ;; (text% treats #\return as a newline so DOS formatted files have extra newlines)
          (send page-text insert (string-replace
                                  (port->string data-port)
                                  "\r\n"
                                  "\n"))
          (send page-text set-position 0)
          (send page-text end-edit-sequence)]
         [(string-prefix? mimetype "image/")
          (define img (make-object image-snip% data-port 'unknown))
          (send page-text begin-edit-sequence)
          (send page-text erase)
          (send page-text insert img)
          (send page-text set-position 0)
          (send page-text end-edit-sequence)]
         [else
          (send page-text begin-edit-sequence)
          (send page-text erase)
          (send page-text insert (format "unknown mimetype: ~a~n" mimetype))
          (send page-text insert (format "Initiating download of ~a~n" (request-path/selector req)))
          (send page-text end-edit-sequence)
          (save-gemini-to-file (gemini-response-data-port resp) (request-path/selector req))]))
     (close-input-port (gemini-response-data-port resp))
     req]
    [(30 31)
     ;; initiate a file download
     (send page-text begin-edit-sequence)
     (send page-text erase)
     (send page-text insert (format "Initiating download of ~a~n" (request-path/selector req)))
     (send page-text end-edit-sequence)
     (save-gemini-to-file (gemini-response-data-port resp) (request-path/selector req))]
    
    [(40) (show-gemini-error "Temporary failure")]
    [(41) (show-gemini-error "Server unavailable")]
    [(42) (show-gemini-error "CGI error")]
    [(43) (show-gemini-error "Proxy error")]
    [(44) (show-gemini-error (string-join "Slow down, wait for " (gemini-response-meta resp) "seconds"))]

    [(50) (show-gemini-error "Permanent failure")]
    [(51) (show-gemini-error "Not found")]
    [(52) (show-gemini-error "Gone")]
    [(53) (show-gemini-error "Proxy request refused")]
    [(54) (show-gemini-error "Bad request")]
    
    [else (show-gemini-error "Unknown status code returned from server")]))

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
      ;; prevent sequence of duplicate URLs. replace top element of history if it
      ;; refers to the same URL/Request. essentially this only updates the current
      ;; selection position.
      (if (and (not (empty? history))
               (equal? (browser-url-req url) (browser-url-req (car history))))
          (set! history (cons url (cdr history)))
          (set! history (cons url history))))

    (define/private (pop-history)
      (if (not (empty? history))
        (let ([top (car history)])
          (set! history (cdr history))
          top)
        '()))

    (define/private (previous-history)
      (if (not (empty? history))
          (car history)
          #f))

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

    (define/private (get-visible-line-count)
      (define start (box 0))
      (define end (box 0))
      (get-visible-line-range start end #f)
      (- (unbox end) (unbox start)))

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
            ;; make the selection visible but don't adjust the screen so the selection isn't at
            ;; the very bottom if the enter page won't fit
            (scroll-to-position 0
                                #f
                                (line-start-position
                                 (+ (position-line initial-selection-pos) 
                                    (floor (/ (get-visible-line-count) 4))))
                                'end)
            ;; scroll to the beginning
            (scroll-to-position 0))))

    (define/public (cancel-request)
      (when (custodian? thread-custodian)
        (custodian-shutdown-all thread-custodian)
        ;; without this the editor gets stuck in no refresh mode
        (when (in-edit-sequence?)
          (end-edit-sequence))
        (set! thread-custodian #f)
        ;; update status message
        (send (get-canvas) update-status "Ready")))
      
    (define/private (load-page req [initial-selection-pos #f] #:back? [back? #f])
      (define (make-history-updater old-url old-position)
        (if back?
            (lambda () (pop-history))
            (lambda ()
              ;; add previous page to history
              (when old-url
                (if old-position
                    ;; also save the position of the selection that we are following so we can return to it
                    (push-history (struct-copy browser-url old-url [selection-pos old-position]))
                    (push-history old-url))))))
      
      ;; this will shutdown the previous custodian on every page load.
      ;; seems wasteful not to re-use the custodian if we aren't actually interrupting
      ;; the previous thread's work.
      (cancel-request)
      (set! thread-custodian (make-custodian))

      (define update-history (make-history-updater current-url
                                                   (if selection
                                                       (get-snip-position selection)
                                                       #f)))
      (send (get-canvas) update-status "Loading...")
      
      (parameterize ([current-custodian thread-custodian])
        (cond
          [(equal? (request-protocol req) 'gopher)
           (thread (thunk
                    (goto-gopher req this initial-selection-pos)
                    (update-history)
                    (set! current-url (browser-url req initial-selection-pos))
                    (send (get-canvas) update-address req)
                    (send (get-canvas) update-status "Ready")))]
          [(equal? (request-protocol req) 'gemini)
           (thread (thunk
                    (define terminal-request (goto-gemini req this))
                    (unless (void? terminal-request)
                      (update-history)
                      (set! current-url (browser-url terminal-request #f))
                      (send (get-canvas) update-address terminal-request)
                      (send (get-canvas) update-status "Ready"))))]
          [else
           ;; TODO display error to user?
           (eprintf "Invalid request protocol!~n")])))
    
    (define/public (go req)
      (define (download-only-type? type)
        (or (equal? type #\5)
            (equal? type #\9)))
      
      (cond
        [(equal? (request-protocol req) 'gemini)
         ;; gemini complicates matters because we must send our request before we know the type of request
         ;; and what we need to do with it. We have to handle this in the request thread instead of before
         ;; creating the thread as we do with gopher.
         (load-page req)]
        ;; Below here is for gopher
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
         (load-page query-request)]
        [(gopher-url-request? req)
         ; URL, probably http, open in external browser
         (eprintf "opening ~a in browser~n" (gopher-url-request->url req))
         (send-url (gopher-url-request->url req) #t)]
        [else
         (load-page req)]))

    (define/public (go-back)
      (unless (empty? history)
        (define prev-url (previous-history))
        (define req (browser-url-req prev-url))
        (load-page req (browser-url-selection-pos prev-url) #:back? #t)))

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

    ;; return a list of requests since browser-url is a private struct
    (define/public (get-history)
      (for/list ([item (in-list history)])
        (browser-url-req item)))
      
    (define/override (on-local-char event)
      (if gopher-menu?
          (case (send event get-key-code)
            [(down)
             (cond
               [(or (not selection) (current-selection-visible?))
                ;; change selection to the next menu snip
                (define item (find-next-menu-snip selection))
                ;(eprintf "browser-text on-local-char down: new selection = ~a~n" item)
                (when item
                  (define pos (get-snip-position item))
                  (change-highlight selection item)
                  (set! selection item)
                  (define start (box 0))
                  (define end (box 0))
                  (get-visible-line-range start end #f)
                  (define visible-lines (- (unbox end) (unbox start)))
                  (define scroll-amount (truncate (* (/ visible-lines 4) 3)))
                  (set-position pos 'same #f #t 'default)
                  (when (>= (position-line pos) (unbox end))
                    ;; scroll down to show the next page
                    (define new-start (+ (unbox start) scroll-amount))
                    (eprintf "scrolling to line ~a-~a~n" new-start (+ new-start visible-lines))
                    (scroll-to-position (line-start-position new-start)
                                        #f
                                        (line-start-position (+ new-start visible-lines))
                                        'end)))]
               [else
                ;; else scroll to make the current selection visible
                (scroll-to-position (get-snip-position selection))])]
            [(up)
             (define item (find-prev-menu-snip selection))
             ;(eprintf "browser-text on-local-char up: new selection = ~a~n" item)
             (when item
               (define pos (get-snip-position item))
               (change-highlight selection item)
               (set! selection item)
               (define start (box 0))
               (define end (box 0))
               (get-visible-line-range start end #f)
               (define visible-lines (- (unbox end) (unbox start)))
               (define scroll-amount (ceiling (* (/ visible-lines 4) 3)))
               (set-position pos 'same #f #t 'default)
               (when (< (position-line pos) (unbox start))
                 ;; scroll up to show the previous page
                 (define new-start (max 0 (- (unbox start) scroll-amount))) ; start position cannot be negative
                 (eprintf "scrolling to line ~a-~a~n" new-start (+ new-start visible-lines))
                 (scroll-to-position (line-start-position new-start)
                                     #f
                                     (line-start-position (+ new-start visible-lines))
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
    (init-field [tab-id 0]
                [update-status-cb #f]
                [update-address-cb #f])
    (inherit get-editor)

    (define status-text "Ready")
    
    (define/public (update-status [text #f])
      (when update-status-cb
        (if text
            (begin
              (set! status-text text)
              (update-status-cb tab-id text))
            (update-status-cb tab-id status-text))))
    
    ;; takes a request struct and updates UI elements using the provided callback
    (define/public (update-address req)
      (when update-address-cb
        (update-address-cb tab-id req)))
    
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

    (define/public (get-history)
      (define editor (get-editor))
      (if editor
          (send editor get-history)
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

(define gemini-link-snip%
  (class string-snip%
    (init-field [url ""])
    (inherit get-flags set-flags get-admin)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    (set-flags (cons 'handles-between-events (get-flags)))

    (define status-text (string-append "=> " url))
    
    (define/override (on-event dc x y editorx editory event)
      ;(eprintf "mouse event ~a~n" (send event get-event-type))
      (when (send event moving?)
        (define canvas (send (send (get-admin) get-editor) get-canvas))
        ;(eprintf "mouse motion event~n")
        (send canvas update-status status-text))
      (super on-event dc x y editorx editory event))

    (define/override (on-goodbye-event dc x y editorx editory event)
      (define canvas (send (send (get-admin) get-editor) get-canvas))
      ;(eprintf "goodbye event~n")
      (send canvas update-status "Ready"))))

(define address-field%
  (class text-field%
    (super-new)
    (inherit get-editor
             has-focus?
             focus)
    ;; can return to this when https://github.com/racket/racket/issues/3883 is released
    #;(define/override (on-focus on?)
      (eprintf "on-focus ~a -> ~a~n" (has-focus?) on?))
    
    #;(define/override (on-subwindow-event recv event)
      (if (send event button-down? 'left)
          (if (has-focus?)
              (begin
                (eprintf "already has focus~n")
                (super on-subwindow-event recv event))
              (begin
                (eprintf "selecting all~n")
                (super on-subwindow-event recv event)
                (focus)
                (send (get-editor) select-all)
                #t))
          (super on-subwindow-event recv event)))))
