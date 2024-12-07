#lang racket/gui

(require net/sendurl)
(require "config.rkt")
(require "request.rkt")
(require "gopher.rkt")
(require "gemini.rkt")
(require "download.rkt")
(require "html.rkt")
(require "dlist.rkt")
(require "layout-canvas.rkt")

(provide browser-canvas%
         menu-item-snip%
         address-field%)

(struct browser-url
  (req
   selection-pos)  ;; position value or #f
  #:prefab)

(define (insert-menu-item canvas dir-entity)
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
      [(#\d) "(DOCU) "]
      [(#\P) " (PDF) "]
      [(#\>) " (EXT) "] ;; special type created for internal use. indicates an option to the user to open a file externally.
      [else  "(UNKN) "]))

  (define standard-style
    (send (send canvas get-style-list) find-named-style "Standard"))
  (define link-style
    (send (send canvas get-style-list) find-named-style "Link"))
  
  (define type-snip (new string-snip%))
  (define link-snip (new menu-item-snip% (dir-entity dir-entity) (browser-canvas canvas)))
  
  (define type-text (gopher-menu-type-text (gopher-dir-entity-type dir-entity)))
  (define display-text (gopher-dir-entity-user-name dir-entity))

  ;; insert text for type indication
  (send type-snip set-style standard-style)
  (send type-snip insert type-text (string-length type-text))
  (send canvas append-snip type-snip)

  (send link-snip set-style link-style)
  (send link-snip insert display-text (string-length display-text)) ;(send link-snip get-count))
  (send canvas append-snip link-snip #t))

(define (insert-directory-line canvas line)
  ;(eprintf "insert-directory-line: ~a~n" line)
  (if (non-empty-string? line)
      (let ([dir-entity (parse-dir-entity line)])
        (cond
          ;; just skip/ignore end of transmission
          [(equal? (string-ref line 0) #\.)
           void]
          ;; display error line
          [(equal? (gopher-dir-entity-type dir-entity) #\3)
           (send canvas append-string (gopher-dir-entity-user-name dir-entity))]
          ;; insert informational lines as plain text
          [(equal? (gopher-dir-entity-type dir-entity) #\i)
           (send canvas append-string "       " #f #f)  ; indent information text to line up with menu items
           (if (= (string-length (gopher-dir-entity-user-name dir-entity)) 0)
               (send canvas append-string "\n")
               (send canvas append-string (gopher-dir-entity-user-name dir-entity)))]
          [else
           (insert-menu-item canvas dir-entity)]))
      ;; be permissive of blank lines
      (send canvas append-string "\n")))

(define (goto-gopher req canvas [initial-selection-pos #f])
  #;(eprintf "goto-gopher: ~a, ~a, ~a, ~a~n" (request-host req) (request-path/selector req) (request-type req) initial-selection-pos)

  (define resp (gopher-fetch (request-host req)
                             (request-path/selector req)
                             (request-type req)
                             (request-port req)))
  
  ;; default the item type to directory if not specified in request
  (define item-type (if (request-type req)
                        (request-type req)
                        #\1))

  ;; reset gopher-menu? boolean to default when loading a new page
  (set-field! gopher-menu? canvas #f)

  (send canvas check-gopher-defaults)
  
  (define update-start-time (current-inexact-monotonic-milliseconds))

  (cond
    [(gopher-response-error? resp)
     (send canvas begin-edit-sequence)
     (send canvas append-string (port->string (gopher-response-data-port resp) #:close? #t))
     (send canvas end-edit-sequence)]
    [(equal? item-type #\1) ; directory
     (send canvas begin-edit-sequence)
     (for ([line (in-lines (gopher-response-data-port resp))])
       (insert-directory-line canvas line))
     (send canvas end-edit-sequence)
     (close-input-port (gopher-response-data-port resp))
     (send canvas init-gopher-menu initial-selection-pos)]
    [(equal? item-type #\0) ; text
     (send canvas begin-edit-sequence)
     (send canvas append-string (port->string (gopher-response-data-port resp)))
     (send canvas end-edit-sequence)
     (close-input-port (gopher-response-data-port resp))]
    [(equal? item-type #\h)
     (send canvas begin-edit-sequence)
     (render-html-to-text (gopher-response-data-port resp)
                          canvas
                          #t
                          #f)
     (send canvas end-edit-sequence)
     (when initial-selection-pos
       (define y (send canvas find-anchor-position initial-selection-pos))
       (and y (send canvas queue-scroll-to y)))
     (close-input-port (gopher-response-data-port resp))]
    [(equal? item-type #\I) ; image
     (define img (make-object image-snip%
                              (gopher-response-data-port resp)
                              'unknown))
     (send canvas begin-edit-sequence)
     (send canvas append-snip img #t)
     (send canvas end-edit-sequence)
     (close-input-port (gopher-response-data-port resp))]
    [(equal? item-type #\g) ; gif
     (define img (make-object image-snip%
                              (gopher-response-data-port resp)
                              'gif))
     (close-input-port (gopher-response-data-port resp))
     (send canvas begin-edit-sequence)
     (send canvas append-snip img #t)
     (send canvas end-edit-sequence)]
    [(or (equal? item-type #\d) ; document (PDF, Word, etc.)
         (equal? item-type #\P))
     (send canvas begin-edit-sequence)
     (send canvas append-string (format "How would you like to handle document ~a ?~n~n" (request-path/selector req)))
     (insert-menu-item canvas
                       (gopher-dir-entity #\> "Open file in external application" (request-path/selector req) (request-host req) (~a (request-port req))))
     (send canvas append-string "\n")
     (insert-menu-item canvas
                       (gopher-dir-entity #\9 "Download file" (request-path/selector req) (request-host req) (~a (request-port req))))
     (send canvas end-edit-sequence)
     (send canvas init-gopher-menu #f)
     (close-input-port (gopher-response-data-port resp))]
    [else
     (send canvas begin-edit-sequence)
     (send canvas append-string (format "Unsupported type ~a~n~n" item-type))
     (insert-menu-item canvas
                       (gopher-dir-entity #\9 "Download file" (request-path/selector req) (request-host req) (~a (request-port req))))
     (send canvas end-edit-sequence)
     (send canvas init-gopher-menu #f)
     (close-input-port (gopher-response-data-port resp))])  
  #;(eprintf "goto-gopher UI update took ~a ms~n" (- (current-inexact-monotonic-milliseconds) update-start-time)))

;; download gopher selector to a temp file and open it with an external application
;; set plumber to clean up file when molasses exits
;; on linux, use 'xdg-open' to find the correct application
;; on mac osx, use 'open'
;; on windows, use shell-execute
(define (open-with-app req)
  (define tmp-file (make-temporary-file "molasses-doc-tmp-~a"))
  (when tmp-file
    (define resp (gopher-fetch (request-host req)
                               (request-path/selector req)
                               (request-type req)
                               (request-port req)))
    ;; download to temp file
    (with-output-to-file tmp-file #:exists 'truncate
      (lambda ()
        #;(eprintf "open-with-app: saving tmp file: ~a" tmp-file)
        (copy-port (gopher-response-data-port resp) (current-output-port))))
    (close-input-port (gopher-response-data-port resp))
    ;; delete the temp file when molasses exits
    (plumber-add-flush! (current-plumber)
                        (lambda (plumber) (delete-file tmp-file)))
    ;; open the file in a system dependent way
    (define platform (system-type))
    (cond 
      [(eq? platform 'windows)
       (shell-execute "open" (path->string tmp-file) "" (current-directory) 'sw_shownormal)]
      [(eq? platform 'unix)
        (define exec-path (find-executable-path "xdg-open"))
        (when exec-path (system* exec-path tmp-file))]
      [(eq? platform 'macosx)
        (define exec-path (find-executable-path "open"))
        (when exec-path (system* exec-path tmp-file))])))

(define (save-gopher-to-file req)
  #;(eprintf "save-gopher-to-file: ~a, ~a, ~a~n" (request-host req) (request-path/selector req) (request-type req))
  ;; get path from user
  (define path (put-file "Save file as..." #f #f (file-name-from-path (string->path (request-path/selector req)))))
  #;(eprintf "saving binary file to ~a~n" path)

  (when path
    (define resp (gopher-fetch (request-host req)
                               (request-path/selector req)
                               (request-type req)
                               (request-port req)))
    (track-download (current-thread) (gopher-response-data-port resp) path)
    (with-output-to-file path #:exists 'truncate
      (lambda ()
        (copy-port (gopher-response-data-port resp) (current-output-port))
        (mark-download-complete (current-thread))))
    (close-input-port (gopher-response-data-port resp))))

(define (save-gemini-to-file data-port remote-path)
  #;(eprintf "save-gemini-to-file~n")
  ;; get path from user
  (define path (put-file "Save file as..." #f #f (file-name-from-path (string->path remote-path))))
  #;(eprintf "saving binary file to ~a~n" path)

  (when path
    (track-download (current-thread) data-port path)
    (with-output-to-file path #:exists 'truncate
      (lambda ()
        (copy-port data-port (current-output-port))
        (mark-download-complete (current-thread))))))

#;(define (selection-clickback-handler text-widget start end)
  (define snip (send text-widget find-snip start 'after))
  (eprintf "clickback: start=~a, snip=~a~n" start snip)
  (when (and snip (is-a? snip menu-item-snip%))
    (set-field! selection text-widget snip)
    (define dir-entity (get-field dir-entity snip))
    (send text-widget go (dir-entity->request dir-entity))))


; Links take the form "=>[<whitespace>]<URL>[<whitespace><USER-FRIENDLY LINK NAME>]<CR><LF>"
(define gemini-link-re #px"^=>\\s*(\\S*)\\s*(.*)")

; Headers start with "#", "##", or "###".
(define gemini-header-re #px"^#{1,3} .*")

(define gemini-pre-re #px"^```.*")

(define (insert-gemini-text canvas data-port base-url)
  (define standard-style
    (send (send canvas get-style-list) find-named-style "Standard"))
  (define link-style
    (send (send canvas get-style-list) find-named-style "Link"))
  (define header1-style
    (send (send canvas get-style-list) find-named-style "Header1"))
  (define header2-style
    (send (send canvas get-style-list) find-named-style "Header2"))
  (define header3-style
    (send (send canvas get-style-list) find-named-style "Header3"))
  
  ; We'll just display them as bold text
  (define (line->header line)  
    (define octothorpe-count
      (if (equal? (string-ref line 1) #\#)
          (if (equal? (string-ref line 2) #\#)
              3
              2)
          1))
    (define header-style
      (case octothorpe-count
        [(1) header1-style]
        [(2) header2-style]
        [(3) header3-style]))
    ;; skip the mandatory space character after the last '#'
    (define header-string (substring line (add1 octothorpe-count)))
    (send canvas append-string header-string header-style #t))
  
  ; Plain text lines are anything else
  (define (line->text line)
    (if (non-empty-string? line)
        (send canvas append-string line standard-style #t)
        ;; be permissive of blank lines
        (send canvas append-string "\n")))
  
  (define (make-link text url base-url)
    (define link-snip (new gemini-link-snip% (url url) (base-url base-url) (browser-canvas canvas)))
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
    #;(eprintf "gemini line=~a~n" line)
    (match line
      [(regexp gemini-link-re)
       (define-values (link-url link-name) (parse-link line))
       ;(eprintf "link url=~a, name=~a~n" link-url link-name)
       (send canvas append-snip (make-link link-name link-url base-url) #t)
       ;; add clickback to link region
       #;(send text-widget set-clickback
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
       (line->header line)]
      [(regexp gemini-pre-re)
       ;; read preformatted text until we read another "```". technically should create
       ;; a style for this that has a fixed width font, but our default font already
       ;; is fixed width.
       (let loop ([line (read-line data-port)])
         (unless (or (regexp-match gemini-pre-re line) (equal? line eof))
           (line->text line)
           (loop (read-line data-port))))]
      [_ (line->text line)])))

;; unlike goto-gopher, goto-gemini returns a request
;; the initial request can be forwaded to a new request for queries
;; 
(define (goto-gemini req canvas)
  #;(eprintf "goto-gemini: ~a, ~a~n" (request-host req) (request-path/selector req))

  (define (show-gemini-error msg)
    (send canvas begin-edit-sequence)
    (send canvas append-string msg)
    (send canvas end-edit-sequence)
    (close-input-port (gemini-response-data-port resp)))
  
  (define resp (gemini-fetch (request-host req)
                             (request-path/selector req)
                             (request-port req)))

  #;(eprintf "goto-gemini: status=~a, meta=~a, from-url=~a~n" (gemini-response-status resp) (gemini-response-meta resp)
           (gemini-response-from-url resp))

  (send canvas check-gemini-defaults)

  ;; the case needs to be in tail position so that it returns the correct value to load-page
  (case (gemini-response-status resp)
    [(10)
     (close-input-port (gemini-response-data-port resp))
     (define query-string (get-text-from-user "Input required" (gemini-response-meta resp)))
     (if query-string
         (let ([query-request (url->request (string-append (gemini-response-from-url resp) "?" query-string))])
           (goto-gemini query-request canvas))
         (void))]
    [(20 21)
     (let ([data-port (gemini-response-data-port resp)]
           [mimetype (gemini-response-meta resp)]
           [from-url (gemini-response-from-url resp)])
       (cond
         [(string-prefix? mimetype "text/gemini")
          (send canvas begin-edit-sequence)
          (insert-gemini-text canvas data-port from-url)
          (send canvas end-edit-sequence)]
         [(string-prefix? mimetype "text/")
          (send canvas begin-edit-sequence)
          ;; this isn't ideal but is still a lot faster than inserting one line at a time
          ;; (text% treats #\return as a newline so DOS formatted files have extra newlines)
          (send canvas append-string (string-replace
                                      (port->string data-port)
                                      "\r\n"
                                      "\n"))
          (send canvas end-edit-sequence)]
         [(string-prefix? mimetype "image/")
          (define img (make-object image-snip% data-port 'unknown))
          (send canvas begin-edit-sequence)
          (send canvas append-snip img #t)
          (send canvas end-edit-sequence)]
         [else
          (send canvas begin-edit-sequence)
          (send canvas append-string (format "unknown mimetype: ~a~n" mimetype))
          (send canvas append-string (format "Initiating download of ~a~n" (request-path/selector req)))
          (send canvas end-edit-sequence)
          (save-gemini-to-file (gemini-response-data-port resp) (request-path/selector req))]))
     (close-input-port (gemini-response-data-port resp))
     req]
    [(30 31)
     ;; initiate a file download
     (send canvas begin-edit-sequence)
     (send canvas append-string (format "Initiating download of ~a~n" (request-path/selector req)))
     (send canvas end-edit-sequence)
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

(define browser-canvas%
  (class layout-canvas% (super-new)
    (init-field [tab-id 0]
                [default-bg-color (make-color 33 33 33)]
                [update-status-cb #f]
                [update-address-cb #f])
    (inherit set-canvas-background
             set-background-image
             get-client-size
             get-drawable-size
             get-virtual-size
             get-view-start
             get-style-list
             get-mode
             set-mode
             set-default-style
             clear-mouse-selection
             begin-edit-sequence
             end-edit-sequence
             in-edit-sequence?
             refresh
             lookup-snip-position-size
             first-visible-snip
             scroll-to
             queue-scroll-to
             redraw-snips)

    (field [current-url #f]
           [gopher-menu? #f]
           [menu-selection (cons 0  #f)]) ; menu item index and dlink for menu item
    
    (define thread-custodian (make-custodian))
    (define request-thread-id #f)
    (define history '())
    (define status-text "Ready")

    ;; keep dlist of gopher menu entries (menu-item-snip%'s)
    (define menu-items (dlist-new))

    (define/public (get-current-request)
      (and current-url (browser-url-req current-url)))

    (define (menu-selection->string selection)
      (if selection
          (send (selection-snip selection) get-item-label)
          "<invalid selection>"))
    
    (define/private (selection-snip selection)
      (if (pair? selection)
          (dlink-value (cdr selection))
          (error "selection-snip: invalid selection")))

    (define/private (selection-visible? selection)
      (define snip (and selection (selection-snip selection)))
      (if snip
          (let-values ([(sx sy sw sh) (lookup-snip-position-size snip)]
                       [(w h) (get-drawable-size)]
                       [(ox oy) (get-view-start)])
            ;; only visible if the entire selection is visible
            ;; gopher menu items selections are only one line, so there shouldn't be edge cases
            (if (and (>= sy oy)
                     (<= (+ sy sh) (+ oy h)))
                #t
                #f))
          #f))

    (define (highlight selection)
      (when selection
        (define snip (selection-snip selection))
        (define new-style (send (get-style-list) find-named-style "Link Highlight"))
        (send snip set-style new-style)))

    (define (unhighlight selection)
      (when selection
        (define snip (selection-snip selection))
        (define new-style (send (get-style-list) find-named-style "Link"))
        (send snip set-style new-style)))

    (define (update-menu-highlight new-selection)
      (begin-edit-sequence)
      (unhighlight menu-selection)
      (set! menu-selection new-selection)
      ;(printf "update-menu-highlight: new selection ~a~n" (menu-selection->string menu-selection))
      (highlight menu-selection)
      (end-edit-sequence)
      (refresh))
    
    ;; return a pair representing the menu selection or #f
    (define/private (find-next-menu-item)
      (define node (and menu-selection
                        (cdr menu-selection)
                        (dlink-next (cdr menu-selection))))
      
      (if node
          (cons (add1 (car menu-selection)) node)
          #f))

    ;; return a pair representing the menu selection or #f
    (define/private (find-prev-menu-item)
      (define node (and menu-selection
                        (cdr menu-selection)
                        (dlink-prev (cdr menu-selection))))
      
      (if node
          (cons (sub1 (car menu-selection)) node)
          #f))

    ;; return a pair representing the menu selection or #f
    ;; index starts at 1, which maps to index 0 in our dlist
    (define/private (find-menu-item index)
      (let loop ([node (dlist-head menu-items)]
                 [i 1])
        (cond
          [(not node) #f]
          [(= i index) (cons index node)]
          [else
           (loop (dlink-next node) (add1 i))])))
    
    ;; return a pair representing the menu selection or #f
    (define/public (find-first-menu-item)
      (if (dlist-head menu-items)
          (cons 1 (dlist-head menu-items))
          #f))

    ;; return a pair representing the menu selection or #f
    (define/private (find-next-visible-menu-item)
      (cond
        [menu-selection
         (define-values (dw dh) (get-drawable-size))
         (define-values (ox oy) (get-view-start))
         
         (let loop ([index (and (cdr menu-selection)
                                (add1 (car menu-selection)))]
                    [node (and (cdr menu-selection)
                               (dlink-next (cdr menu-selection)))])
           (if node
               (let-values ([(x y w h) (lookup-snip-position-size (dlink-value node))])
                 (cond
                   [(< y oy)
                    (loop (add1 index) (dlink-next node))]
                   [(> y (+ oy dh))
                    #f]
                   [else
                    (cons index node)]))
               #f))]
        [else #f]))

    ;; return a pair representing the menu selection or #f
    (define/private (find-prev-visible-menu-item)
      (cond
        [menu-selection
         (define-values (dw dh) (get-drawable-size))
         (define-values (ox oy) (get-view-start))
         
         (let loop ([index (and (cdr menu-selection)
                                (sub1 (car menu-selection)))]
                    [node (and (cdr menu-selection)
                               (dlink-prev (cdr menu-selection)))])
           (if node
               (let-values ([(x y w h) (lookup-snip-position-size (dlink-value node))])
                 (cond
                   [(< y oy)
                    #f]
                   [(>= y (+ oy dh))
                    (loop (sub1 index) (dlink-prev node))]
                   [else
                    (cons index node)]))
               #f))]
        [else #f]))

    (define/override (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      (when (is-a? s menu-item-snip%)
        (dlist-append! menu-items s))
      (super append-snip s end-of-line alignment properties))

    (define/override (erase)
      (set! menu-items (dlist-new))
      (super erase))
    
    (set-canvas-background default-bg-color)

    (define/public (reset-background)
      (set-canvas-background default-bg-color)
      (set-background-image #f))

    (define/public (check-gopher-defaults)
      ;; reset canvas mode if it was changed when loading html
      (when (eq? (get-mode) 'layout)
        #;(printf "check-gopher-defaults~n")
        (set-default-style "Standard")
        (reset-background)
        (set-mode 'plaintext)))

    (define/public (check-gemini-defaults)
      ;; reset canvas mode if it was changed when loading html
      (when (eq? (get-mode) 'layout)
        (set-default-style "Standard")
        (reset-background)
        (set-mode 'wrapped)))
    
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

    (define/public (init-gopher-menu [initial-selection-pos #f])
      (set! gopher-menu? #t)
      (if initial-selection-pos
          (set! menu-selection (find-menu-item initial-selection-pos)) 
          (set! menu-selection (find-first-menu-item)))
      (when (and menu-selection (cdr menu-selection))
        #;(eprintf "highlight first menu item~n")
        (define snip (selection-snip menu-selection))
        (define new-style (send (get-style-list) find-named-style "Link Highlight"))
        (send snip set-style new-style)
        (if initial-selection-pos
            ;; make the selection visible but not at the very top
            (let-values ([(x y w h) (lookup-snip-position-size snip)]
                         [(cw ch) (get-client-size)])
              (queue-scroll-to (max 0 (- y (floor (/ ch 4))))))
            ;; scroll to the beginning
            (queue-scroll-to 0))))

    (define/public (cancel-request)
      (printf "cancel request called~n")
      (when (custodian? thread-custodian)
        (update-status "Cancelling...")
        (printf "cancelling request: ~a~n" (custodian-managed-list thread-custodian (current-custodian)))
        (custodian-shutdown-all thread-custodian)
        ;; without this the canvas could get stuck in no refresh mode
        (when (in-edit-sequence?)
          (end-edit-sequence))
        (set! thread-custodian (make-custodian))
        ;; update status message
        (update-status "Ready")))
    
    (define/private (load-page req [initial-selection-pos #f] #:back? [back? #f])
      (define (make-history-updater old-url old-position)
        ;(printf "old-position = ~a~n" old-position)
        (if back?
            (lambda () (pop-history))
            (lambda ()
              ;; add previous page to history
              (when old-url
                (if old-position
                    ;; also save the position of the selection that we are following so we can return to it
                    (push-history (struct-copy browser-url old-url [selection-pos old-position]))
                    (push-history old-url))))))

      ;; background color can be changed by html rendering
      (reset-background)

      (clear-mouse-selection)

      ;; shutdown the previous request thread if it hasn't finished 
      ;; I think we only need to shutdown the custodian if the thread never completed
      (when (and (thread? request-thread-id)
                 (thread-running? request-thread-id))
        (cancel-request))

      (define update-history (make-history-updater current-url (and menu-selection (car menu-selection))))

      (update-status "Loading...")
      
      (parameterize ([current-custodian thread-custodian])
        (cond
          [(equal? (request-protocol req) 'gopher)
           (update-history)
           (set! current-url (browser-url req initial-selection-pos))
           (update-address req)
           ;; clear the current page contents
           (begin-edit-sequence)
           (erase)
           (end-edit-sequence)
           (set! request-thread-id
                 (thread (thunk
                          (goto-gopher req this initial-selection-pos)
                          (update-status "Ready"))))]
          [(equal? (request-protocol req) 'gemini)
           (update-history)
           (set! current-url (browser-url req #f))
           (update-address req)
           ;; clear the current page contents
           (begin-edit-sequence)
           (erase)
           (end-edit-sequence)
           (thread (thunk
                    (define terminal-request (goto-gemini req this))
                    (unless (void? terminal-request)
                      (set! current-url (browser-url terminal-request #f))
                      (update-address terminal-request)
                      (update-status "Ready"))))]
          [else
           ;; TODO display error to user?
           (eprintf "Invalid request protocol!~n")])))
    
    (define/public (go req [initial-selection-pos #f])
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
        [(equal? (request-type req) #\>) ; open file in external application
         (thread (thunk
                  (open-with-app req)))]
        [(equal? (request-type req) #\7) ; gopher index search
         ;; prompt user for query string
         (define query-string (get-text-from-user "Query" "search string"))
         (when query-string
           (define query-request (request (request-protocol req)
                                          (request-host req)
                                          (request-port req)
                                          (string-append (request-path/selector req) "\t" query-string)
                                          #\1))
           (load-page query-request))]
        [(gopher-url-request? req)
         ; URL, probably http, open in external browser
         #;(eprintf "opening ~a in browser~n" (gopher-url-request->url req))
         (send-url (gopher-url-request->url req) #t)]
        [(equal? (request-type req) #\h) ; html file
         (load-page req initial-selection-pos)]
        [else
         (load-page req)]))

    (define/public (go-back)
      (unless (empty? history)
        (define prev-url (previous-history))
        (define req (browser-url-req prev-url))
        (load-page req (browser-url-selection-pos prev-url) #:back? #t)))
    
    (define/public (load-restore-data list-of-data)
      (cond
        [(and (list? list-of-data)
              (= (length list-of-data) 2))
         ;; old-style config file, will be replaced with new version
         #;(printf "loading old-style tabs save data~n")
         (set! history (cadr list-of-data))
         (when (browser-url? (car list-of-data))
           (go (browser-url-req (car list-of-data))))]
        [else
         (define history-saved (assq 'history list-of-data))
         (define url-saved (assq 'current-url list-of-data))
         (define mode-saved (assq 'mode list-of-data))
         
         (when history-saved
           #;(printf "loading tab history: ~a~n" (cadr history-saved))
           (set! history (cadr history-saved)))
         
         (when mode-saved
           #;(printf "loading tab mode: ~a~n" (cadr mode-saved))
           (set-mode (cadr mode-saved)))
         
         (when (and url-saved (browser-url? (cadr url-saved)))
           #;(printf "loading tab url: ~a~n" (cadr url-saved))
           (go (browser-url-req (cadr url-saved))))]))
    
    (define/public (get-restore-data)
      `((current-url ,current-url) (history ,history) (mode ,(get-mode))))

    (define/public (get-history)
      (for/list ([item (in-list history)])
        (browser-url-req item)))

    (define/override (on-event event)
      (super on-event event))

    (define (lines-visible height line-height)
      (truncate (/ height line-height)))

    (define (snips-to-redraw old-selection new-selection)
      (if old-selection
            (list (dlink-value (cdr old-selection)) (dlink-value (cdr new-selection)))
            (list (dlink-value (cdr new-selection)))))
    
    (define (change-menu-selection direction item)
      (update-menu-highlight item)
      (when (not (selection-visible? menu-selection))
        (let-values ([(sx sy sw sh) (lookup-snip-position-size (selection-snip item))]
                     [(w h) (get-drawable-size)]
                     [(vx vy) (get-virtual-size)])
          (cond
            [(eq? direction 'down)
             (define max-y (- vy h))
             (define new-y (min max-y (- sy (/ h 4))))
             (scroll-to new-y canvas-smooth-scrolling)
             ;; nudge the visible area just a bit so that the first line isn't partially cut off
             (define-values (fsx fsy fsw fsh) (lookup-snip-position-size (first-visible-snip)))
             (when (and (< fsy new-y) (< new-y max-y))
               (scroll-to fsy #f))]
            [(eq? direction 'up)
             (define new-y (max 0 (- sy (* (/ h 4) 3))))
             (scroll-to new-y canvas-smooth-scrolling)
             ;; nudge the visible area just a bit so that the first line isn't partially cut off
             (define-values (fsx fsy fsw fsh) (lookup-snip-position-size (first-visible-snip)))
             (when (and (< fsy new-y) (> new-y 0))
               (scroll-to fsy #f))]
            [else
             (error "change-selection: invalid direction")]))))
    
    (define/override (on-char event)
      (if gopher-menu?
          (case (send event get-key-code)
            [(down)
             (cond
               [(not menu-selection)
                (define item (find-first-menu-item))
                (when item
                  #;(eprintf "browser-text on-char down: new selection = ~a:~a~n" (car item) (send (selection-snip item) get-item-label))
                  (change-menu-selection 'down item))]
               [(or (not (cdr menu-selection)) (selection-visible? menu-selection))
                ;; change selection to the next menu snip
                (define item (find-next-menu-item))
                (when item
                  #;(eprintf "browser-text on-char down: new selection = ~a:~a~n" (car item) (send (selection-snip item) get-item-label))
                  (change-menu-selection 'down item))]
               [else
                ;; just scroll to make the selected menu snip visible
                (define-values (sx sy sw sh) (lookup-snip-position-size (selection-snip menu-selection)))
                (define-values (w h) (get-drawable-size))
                (scroll-to (- sy (/ h 4)) canvas-smooth-scrolling)])]
            [(up)
             (define item (find-prev-menu-item))
             (when item
               #;(eprintf "browser-text on-char up: new selection = ~a:~a~n" (car item) (send (selection-snip item) get-item-label))
               (change-menu-selection 'up item))]
            [(left)
             (go-back)]
            [(right #\return)
             (when menu-selection
               (go (dir-entity->request (get-field dir-entity (selection-snip menu-selection)))))]
            [(next)
             (define-values (x y) (get-view-start))
             (define-values (w h) (get-drawable-size))
             (define-values (vx vy) (get-virtual-size))
             (define max-y (- vy h))
             (define new-y (min max-y (+ y h)))
             (scroll-to new-y canvas-smooth-scrolling)
             ;; nudge the visible area just a bit so that the first line isn't partially cut off
             (define-values (fsx fsy fsw fsh) (lookup-snip-position-size (first-visible-snip)))
             (when (and (< fsy new-y) (< new-y max-y))
               (scroll-to fsy #f))
             (unless (selection-visible? menu-selection)
               (define item (find-next-visible-menu-item))
               (when item
                 (update-menu-highlight item)))]
            [(prior)
             (define-values (x y) (get-view-start))
             (define-values (w h) (get-drawable-size))
             (define new-y (max 0 (- y h)))
             (scroll-to new-y canvas-smooth-scrolling)
             ;; nudge the visible area just a bit so that the first line isn't partially cut off
             (define-values (fsx fsy fsw fsh) (lookup-snip-position-size (first-visible-snip)))
             (when (and (< fsy new-y) (> new-y 0))
               (scroll-to fsy #f))
             (unless (selection-visible? menu-selection)
               (define item (find-prev-visible-menu-item))
               (when item
                 (update-menu-highlight item)))]
            [(home)
             (scroll-to 0 canvas-smooth-scrolling)
             (unless (selection-visible? menu-selection)
               (define item (find-first-menu-item))
               (when item
                 (update-menu-highlight item)))]
            [(end)
             (define-values (vx vy) (get-virtual-size))
             (define-values (dw dh) (get-drawable-size))
             (scroll-to (- vy dh) canvas-smooth-scrolling)
             (unless (selection-visible? menu-selection)
               (define item (find-next-visible-menu-item))
               (when item
                 (update-menu-highlight item)))]
            [else
             (define key-code (send event get-key-code))
             (super on-char event)])
          (case (send event get-key-code)
            [(left)
             (go-back)]
            [(right)
             (void)]
            #;[(up)
             ;; scroll the screen up one line
             (define start (box 0))
             (define end (box 0))
             (get-visible-line-range start end #f)
             (define new-line (max 0 (sub1 (unbox start))))
             ;(eprintf "range (~a,~a): new line = ~a~n" (unbox start) (unbox end) new-line)
             (set-position (line-start-position new-line))]
            #;[(down)
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
             (super on-char event)])))
    ))

(define menu-item-snip%
  (class string-snip%
    (init-field [dir-entity #f]
                [browser-canvas #f])
    (inherit get-flags set-flags)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    ;(set-flags (cons 'handles-events (get-flags)))

    (define/public (get-item-label)
      (gopher-dir-entity-user-name dir-entity))
    
    (define (follow-link)
      (send browser-canvas go (dir-entity->request dir-entity)))
    
    (define/override (on-event dc x y editorx editory e)
      #;(eprintf "menu-item-snip% on-event~n")
      (when (send e button-down? 'left)
        (follow-link)))))

(define gemini-link-snip%
  (class string-snip%
    (init-field [url ""]
                [base-url ""]
                [browser-canvas #f])
    (inherit get-flags set-flags get-admin)
    (super-new)
    (set-flags (cons 'handles-all-mouse-events (get-flags)))
    (set-flags (cons 'handles-between-events (get-flags)))

    (define status-text (string-append "=> " url))

    (define (replace-final-path-element path relative-path)
      (string-append (path->string (path-only path)) relative-path))

    (define (follow-link)
      #;(eprintf "following gemini link: ~a~n" url)
      (if (regexp-match #px"^(\\w+://).*" url)
          (cond
            [(string-prefix? url "gemini://")
             (send browser-canvas go (url->request url))]
            [(string-prefix? url "gopher://")
             (send browser-canvas go (url->request url))]
            [(or (string-prefix? url "http://")
                 (string-prefix? url "https://"))
             (send-url url #t)])
          ;; handle partial URLs
          (let ([base-req (url->request base-url)])
            (send browser-canvas go
                  (struct-copy request
                               base-req
                               [path/selector
                                (if (equal? (string-ref url 0) #\/)
                                    url
                                    (replace-final-path-element (request-path/selector base-req) url))])))))

    (define/override (on-event dc x y editorx editory event)
      #;(eprintf "gemini-link-snip% mouse event ~a~n" (send event get-event-type))
      (cond
        [(send event moving?)
         ;(eprintf "mouse motion event~n")
         (send browser-canvas update-status status-text)]
        [(send event button-down? 'left)
         (follow-link)]))

    (define/override (on-goodbye-event dc x y editorx editory event)
      ;(eprintf "goodbye event~n")
      (send browser-canvas update-status "Ready"))))

;; implements a text field that auto selects its contents when it gains focus
(define address-field%
  (class text-field%
    (super-new)
    (inherit get-editor
             has-focus?
             focus)

    (define last-focus? #f)
    (define lost-focus-via-right-click? #f)
    
    (define/override (on-focus on?)
      ;; only place to do something when focus leaves the text field
      #;(eprintf "on-focus ~a -> ~a~n" last-focus? on?)
      (unless on?
        (if lost-focus-via-right-click?
            (set! lost-focus-via-right-click? #f)
            (begin
              (set! last-focus? #f)
              (send (get-editor) set-position 0)))))
    
    (define/override (on-subwindow-event recv event)
      (define event-type (send event get-event-type))
      (cond
        [(eq? event-type 'left-down)
         (if last-focus?
             (begin
               #;(eprintf "already has focus~n")
               (super on-subwindow-event recv event))
             (begin
               #;(eprintf "selecting all~n")
               (super on-subwindow-event recv event)
               (set! last-focus? #t)
               (send (get-editor) select-all)
               #t))]
        [(eq? event-type 'right-down)
         (if last-focus?
             (super on-subwindow-event recv event)
             (begin
               (set! last-focus? #t)
               (super on-subwindow-event recv event)))]
        ;; following a right-up event the popup menu will appear and take focus
        ;; away from the text field.
        [(eq? event-type 'right-up)
         (if last-focus?
             (begin
               (set! lost-focus-via-right-click? #t)
               (super on-subwindow-event recv event))
             (super on-subwindow-event recv event))]
        [else
         (super on-subwindow-event recv event)]))))
