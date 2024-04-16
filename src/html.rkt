#lang racket/gui

#;(require (except-in html make-object)
           xml
           net/url)

(require (except-in html make-object)
         xml
         sxml
         net/url)

(require "third-party/html-parsing/html-parsing.rkt")
;(require html-parsing)

(require "config.rkt"
         "html-snips.rkt"
         "table.rkt"
         "gopher.rkt"
         "request.rkt")

;; use browser/htmltext frome drracket as a starting point and basis for the api
;; 

;(require "bullet.rkt")
;(require "entity-names.rkt")
;(require "option-snip.rkt")

;; (define sxml (parse-html (open-input-file "/data/jonathan/reversing/fraviaweb2/tek1.htm")))

(provide render-html-to-text)

(define font-size-vec #(7/12 5/6 1.0 1.125 1.5 2.0 3.0))  ; 7 10 12 13.5 18 24 36

(define delta:subscript
  (let ([d (make-object style-delta%)])
    (send d set-alignment-on 'bottom)
    (send d set-size-mult 0.8)
    d))
(define delta:superscript
  (let ([d (make-object style-delta%)])
    (send d set-alignment-on 'top)
    (send d set-size-mult 0.8)
    d))
(define delta:small
  (let ([d (make-object style-delta%)])
    (send d set-size-mult 0.75)
    d))
(define delta:symbol (make-object style-delta% 'change-family 'symbol))

;; for future use with dynamic-instantiate
(define (non-default-attrs attr-list)
  (for/list ([e (in-list attr-list)]
             #:when (cadr e))
    e))

;; create a new bitmap
;; use img src attribute and request structure to compose a gopher request to the image
;; 
(define (load-new-bitmap src request)
  (define selector
    (if (equal? (string-ref src 0) #\/)
        src
        ; src is a relative path
        (string-replace (request-path/selector request)
                        (last (string-split (request-path/selector request) "/"))
                        src)))
  (define response (gopher-fetch (request-host request)
                                 selector
                                 "I"
                                 (request-port request)))
  (with-handlers
    ([exn:fail?
      (lambda (exn)
        (close-input-port (gopher-response-data-port response))
        ; hack to generate the 'missing image' bitmap. don't know how to reference this bitmap directly.
        (make-object bitmap% "/invalid/path/a40aiduuhsth3"))])
    (define new-bitmap (make-object bitmap%
                                    (gopher-response-data-port response)
                                    'unknown))
    (close-input-port (gopher-response-data-port response))
    new-bitmap))

#;(define (join-strings c)
  (let loop ([accum-s (string-normalize-spaces (car c))]
             [l (cdr c)])
    (cond
      [(string?  (car l))
       (loop (string-append accum-s " " (string-normalize-spaces (car l)))
             (cdr l))]
      [else
       (string-normalize-spaces accum-s)])))

(define (join-strings c)
  (string-normalize-spaces (string-join (takef c string?)) #:trim? #f))

(define (skip-strings c)
  (dropf c string?))

(define (fixup-newlines2 c)
  (cond
    [(and (pair? c) (eq? (car c) 'pre))
     (fixup-newlines c)]
    [(and (pair? c) (string? (car c)))
     (define s (join-strings c))
     (if (non-empty-string? s)
         (cons s
               (fixup-newlines2 (skip-strings c)))
         (fixup-newlines2 (skip-strings c)))]
    [(pair? c)
     (cons (fixup-newlines2 (car c))
           (fixup-newlines2 (cdr c)))]
    [else
     c]))

(define (fixup-newlines c)
  (cond
    [(string? c)
      (string-replace c "\r\n" "\n")]
    [(pair? c)
     (cons (fixup-newlines (car c))
           (fixup-newlines (cdr c)))]
    [else
     c]))

(define (read-html2 a-port)
  (let* ([xml (parameterize ([read-html-comments #t]
                             [use-html-spec #f])
                (read-html-as-xml a-port))]
         [xexpr (map xml->xexpr xml)])
    (car xexpr)))

(define (parse-color c-string)
  (cond
    [(equal? (string-ref c-string 0) #\#)
     (make-color (string->number (substring c-string 1 3) 16)
                 (string->number (substring c-string 3 5) 16)
                 (string->number (substring c-string 5 7) 16))]
    [(and (= (string-length c-string) 6)
          (string->number (string-append "#x" c-string)))
     (make-color (string->number (substring c-string 0 2) 16)
                 (string->number (substring c-string 2 4) 16)
                 (string->number (substring c-string 4 6) 16))]
    [else
     (match c-string
       [(regexp "(?i:black)")   (make-color #x0 #x0 #x0)]
       [(regexp "(?i:silver)")  (make-color #xC0 #xC0 #xC0)]
       [(regexp "(?i:gray)")    (make-color #x80 #x80 #x80)]
       [(regexp "(?i:white)")   (make-color #xFF #xFF #xFF)]
       [(regexp "(?i:maroon)")  (make-color #x80 #x0 #x0)]
       [(regexp "(?i:red)")     (make-color #xFF #x0 #x0)]
       [(regexp "(?i:purple)")  (make-color #x80 #x0 #x80)]
       [(regexp "(?i:fuschia)") (make-color #xFF #x0 #xFF)]
       [(regexp "(?i:green)")   (make-color #x0 #x80 #x0)]
       [(regexp "(?i:lime)")    (make-color #x0 #xFF #x0)]
       [(regexp "(?i:olive)")   (make-color #x80 #x80 #x0)]
       [(regexp "(?i:yellow)")  (make-color #xFF #xFF #x0)]
       [(regexp "(?i:navy)")    (make-color #x0 #x0 #x80)]
       [(regexp "(?i:blue)")    (make-color #x0 #x0 #xFF)]
       [(regexp "(?i:teal)")    (make-color #x0 #x80 #x80)]
       [(regexp "(?i:aqua)")    (make-color #x0 #xFF #xFF)]
       [_
        (define named-color (send the-color-database find-color c-string))
        (if named-color
            named-color
            (make-color 16 16 16))])]))

;; read the font size string and return a new font size
(define (parse-font-size size-string cur-size)
  (define val (string->number size-string))
  (cond
    [(or (char=? (string-ref size-string 0) #\+)
         (char=? (string-ref size-string 0) #\-))
     (define new-size (+ cur-size val))
     (if (<= 1 new-size 7)
         new-size
         (if (< new-size 1)
             1
             7))]
    [(and (>= val 1) (<= val 7))
     val]
    [else
     (if (<= 1 val 7)
         val
         (if (< val 1)
             1
             7))]))

(define (read-html a-port)
  (html->xexp a-port))

(define (parse-html a-port)
  (let ([raw (read-html a-port)])
    (fixup-newlines2 raw)))

(define attr-list? (ntype?? '@))

(define (walk-sxml s)
  (unless (empty? s)
    (define node (car s))
    (cond
      [(sxml:element? node)
       (printf "element ~a~n" (sxml:element-name node))
       (walk-sxml (cdr node))
       (walk-sxml (cdr s))]
      [(attr-list? node)
       (printf "atrributes ~a~n" (cdr node))
       (walk-sxml (cdr s))]
      [else
       (printf "skip ~a~n" node)
       (walk-sxml (cdr s))])))

(define null-container%
  (class object% (super-new)
    (define/public (start-cell #:colspan [colspan 1] #:valign [valign 'middle] #:bgcolor [bgcolor #f] #:width [width #f])
      void)
    (define/public (end-cell)
      void)
    (define/public (start-row)
      void)
    (define/public (end-row)
      void)
    (define/public (finalize-table layout-width)
      void)
    (define/public (append-string s [style #f] [end-of-line #t] [alignment 'unaligned] [properties '()])
      void)
    (define/public (append-snip s [end-of-line #f] [alignment 'unaligned] [properties '()])
      void)))

(define (next-bullet-style style)
  (case style
    [(disc) 'circle]
    [(circle) 'square]
    [(square) 'disc]
    [else 'disc]))

(define (convert-html a-port canvas img-ok?)
  (define content (parse-html a-port))
  (define style-list (send canvas get-style-list))
  (define html-basic-style
    (or (send style-list find-named-style "Html Standard")
        (send style-list find-named-style "Standard")
        (send style-list find-named-style "Basic")))
  (define current-style (make-parameter html-basic-style))
  (define current-style-delta (make-parameter (make-object style-delta% 'change-nothing)))
  (define current-block (make-parameter #f))
  (define current-container (make-parameter (make-object null-container%)))
  (define current-bullet-style (make-parameter #f))
  (define current-ol-number (make-parameter #f))
  ;; alignment is a special case since it isn't controlled by a style and must be applied to each paragraph
  (define current-alignment 'unaligned)  
  ;; don't use a parameter for link color since it will change so rarely
  (define current-link-color html-link-color)
  (define current-vlink-color html-vlink-color)
  (define current-font-size 3)

  (define horz-inset (send canvas horizontal-inset))

  ;; need to abstract functions which add elements in order to handle tables
  (define (append-string s . rest-args)
    (if (not (is-a? (current-container) null-container%))
        (send/apply (current-container) append-string s `(,@rest-args))
        (send/apply canvas append-string s `(,@rest-args))))

  (define (append-snip s . rest-args)
    (if (not (is-a? (current-container) null-container%))
        (send/apply (current-container) append-snip s `(,@rest-args))
        (send/apply canvas append-snip s `(,@rest-args))))

  (define (append-anchor name)
    (when name
      ;; create an empty string to serve as the anchor point
      #;(printf "************ ~a~n" (list (cons 'anchor name)))
      (append-string "" #f #f current-alignment (list (cons 'anchor name)))))

  (define (get-container-width canvas)
    (cond
      [(is-a? (current-container) null-container%)
       (send canvas layout-space-on-current-line)]
      [(is-a? (current-container) table-snip%)
       (define-values (dw dh) (send canvas get-drawable-size))
       (define w (send (current-container) estimate-current-cell-width))
       #;(printf "estimated cell width is ~a~n" w)
       (if (> w 0)
           (min w dw) ; estimate uses max width, so constrain if too large
           (* (send canvas layout-space-on-current-line) 0.75))]))
  
  (define (last-element-eol?)
    (if (not (is-a? (current-container) null-container%))
        (send (current-container) last-element-eol?)
        (send canvas last-element-eol?)))

  (define (last-element-ews?)
    (if (not (is-a? (current-container) null-container%))
        (send (current-container) last-element-ews?)
        (send canvas last-element-ews?)))
  
  (define (followed-by-newline? node)
    ;(eprintf "followed-by-newline? ~a~n" node)
    (cond
      [(empty? node) #f]
      [(sxml:element? node)
       (or (eq? (sxml:element-name node) 'br)
           (eq? (sxml:element-name node) 'hr))]
      [else
       #f]))
  
  (define (insert-newline)
    #;(eprintf "inserting newline~n")
    (define style (send style-list find-or-create-style (current-style) (current-style-delta)))
    (append-string "\n" style #t current-alignment))

  (define (start-new-paragraph)
    (when (not (last-element-eol?))
      #;(eprintf "starting new paragraph~n")
      (insert-newline)))

  (define (last-node-in-paragraph? s)
    (and (is-paragraph-element? (current-block))
         (empty? (cdr s))))
  
  (define (is-block-element? elem)
    ;; treat head, title, and body as blocks even though they aren't technically block elements
    ;; head and title need special treatment but body currently doesn't
    (define block-elements '(head title body h1 h2 h3 h4 h5 h6 p pre div center))
    (memq elem block-elements))

  ;; defines elements that create paragraphs and require a newline or two after closing
  ;; may eventually get rid of this function
  (define (is-paragraph-element? elem)
    (define para-elements '(h1 h2 h3 h4 h5 h6 p pre))
    (memq elem para-elements))

  (define (is-text-element? elem)
    (define text-elements '(a b u i big font))
    (memq elem text-elements))

  (define (update-block current-block elem)
    (if (is-block-element? elem)
        elem
        current-block))

  (define (sxml:attr-safer obj attr-name)
    (define attrib (assq attr-name (sxml:attr-list obj)))
    (cond 
      [(and (pair? attrib)
            (not (empty? (cdr attrib))))
       (cadr attrib)]
      [else #f]))

  (define (attr->number node attr)
    (define attr-string (sxml:attr-safer node attr))
    (if attr-string
        (string->number attr-string)
        #f))

  ; returns #f if no width attribute
  (define (width-attr node)
    (define width-value (sxml:attr-safer node 'width))
    (and width-value
         (if (string-contains? width-value "%")
             (cons 'width-percent
                   (string->number (car (string-split width-value "%"))))
             (cons 'width-pixels
                   (string->number width-value)))))
  
  (define (align-attr node default-alignment)
    (case (and (sxml:attr-safer node 'align)
               (string-downcase (sxml:attr node 'align)))
      [("left") 'left]
      [("right") 'right]
      [("center") 'center]
      [else default-alignment]))

  (define (valign-attr node default-alignment)
    (case (and (sxml:attr-safer node 'valign)
               (string-downcase (sxml:attr node 'valign)))
      [("top") 'top]
      [("middle") 'middle]
      [("bottom") 'bottom]
      [("baseline") (error "valign baseline not supported")]
      [else default-alignment]))
  
  (define (rules-attr node default-rules)
    (case (and (sxml:attr-safer node 'rules)
               (string-downcase (sxml:attr node 'rules)))
      [("none") 'none]
      [("all") 'all]
      [("groups") (error "rules groups not supported")]
      [("rows") 'rows]
      [("cols") 'cols]
      [else default-rules]))

  (define (handle-body-attributes node)
    (for/list ([attr (in-list (sxml:attr-list node))])
      #;(eprintf "handling body attribute ~a~n" attr)
      (case (car attr)
        [(bgcolor)
         (define bg-color (parse-color (cadr attr)))
         (send canvas set-canvas-background bg-color)
         (send (current-style-delta) set-delta-background bg-color)
         (lambda () void)]
        [(text)
         (define text-color (parse-color (cadr attr)))
         (send (current-style-delta) set-delta-foreground text-color)
         (lambda () void)]
        [(link)
         (define prev-link-color current-link-color)
         (set! current-link-color (parse-color (cadr attr)))
         (lambda ()
           (set! current-link-color prev-link-color))]
        [(vlink)
         (define prev-vlink-color current-vlink-color)
         (set! current-vlink-color (parse-color (cadr attr)))
         (lambda ()
           (set! current-vlink-color prev-vlink-color))]
        [else
         #;(eprintf "  null func for attribute ~a~n" attr)
         (lambda () void)])))
  
  (define (handle-font-attributes node)
    (define size-value (sxml:attr-safer node 'size))
    (define color-value (sxml:attr-safer node 'color))

    ;; changes current style only, no need for close tag function
    (when color-value
      #;(eprintf "setting font color to ~a~n" color-value)
      (define text-color (parse-color color-value))
      (send (current-style-delta) set-delta-foreground text-color))
    
    (if size-value
        (let ([prev-size current-font-size]
              [size (parse-font-size size-value current-font-size)])
          #;(eprintf "new font size: ~a -> ~a -> ~a~n" prev-size size-value size)
          (set! current-font-size size)
          (send (current-style-delta) set-size-mult (vector-ref font-size-vec (sub1 size)))
          (list (lambda ()
                  (set! current-font-size prev-size))))
        '()))

  (define (handle-big)
    (define prev-size current-font-size)
    (set! current-font-size (min 7 (add1 current-font-size)))
    (send (current-style-delta) set-size-mult (vector-ref font-size-vec (sub1 current-font-size)))
    (list (lambda ()
            (set! current-font-size prev-size))))

  (define (handle-small)
    (define prev-size current-font-size)
    (set! current-font-size (max 1 (sub1 current-font-size)))
    (send (current-style-delta) set-size-mult (vector-ref font-size-vec (sub1 current-font-size)))
    (list (lambda ()
            (set! current-font-size prev-size))))

  (define (handle-paragraph-attributes node)
    (define value (sxml:attr-safer node 'align))
    (if value
        (let ([prev-alignment current-alignment])
          (set! current-alignment (align-attr node current-alignment))
          (list insert-newline
                (lambda ()
                  (set! current-alignment prev-alignment))))
        (list insert-newline)))

  (define (handle-div-attributes node)
    (define value (sxml:attr-safer node 'align))
    (if value
        (let ([prev-alignment current-alignment])
          (set! current-alignment (align-attr node current-alignment))
          (list (lambda ()
                  (set! current-alignment prev-alignment))))
        '()))

  (define (handle-img node [url #f] [base-req #f] [name-value #f])
    (when img-ok?
      (define src-value (sxml:attr-safer node 'src))
      (define request (send canvas get-current-request))
      (when request
        (define align (align-attr node current-alignment))
        (define hspace-value (attr->number node 'hspace))
        (define vspace-value (attr->number node 'vspace))
        #;(printf "handle-img: hs=~a, vs=~a, src=~a~n" hspace-value vspace-value src-value)
        (define bm (load-new-bitmap src-value request))
        (define snip (if url
                         (new html-link-img-snip% (url url) (base-req base-req) (browser-canvas canvas)
                              (hspace (or hspace-value 2)) (vspace (or vspace-value 0)))
                         (new html-image-snip% (hspace (or hspace-value 2)) (vspace (or vspace-value 0)))))
        (send snip set-bitmap bm)
        (append-snip snip #f align (if name-value `((anchor . ,name-value)) '())))))
  
  ;; handle each element based on the element type
  ;; return a list of functions to call when closing the element
  (define (handle-element node)
    (case (car node)
      [(p)
       (start-new-paragraph)
       (handle-paragraph-attributes node)]
      [(b)
       (send (current-style-delta) set-delta 'change-bold)
       '()]
      [(i)
       (send (current-style-delta) set-delta 'change-italic)
       '()]
      [(u)
       (send (current-style-delta) set-delta 'change-underline #t)
       '()]
      [(h1)
       (start-new-paragraph)
       (send (current-style-delta) set-delta 'change-bold)
       (send (current-style-delta) set-size-mult 2.0)
       (handle-paragraph-attributes node)]
      [(h2)
       (start-new-paragraph)
       (send (current-style-delta) set-delta 'change-bold)
       (send (current-style-delta) set-size-mult 1.5)
       (handle-paragraph-attributes node)]
      [(h3)
       (start-new-paragraph)
       (send (current-style-delta) set-delta 'change-bold)
       (send (current-style-delta) set-size-mult 1.2)
       (handle-paragraph-attributes node)]
      [(h4 h5 h6)
       (start-new-paragraph)
       (send (current-style-delta) set-delta 'change-bold)
       (handle-paragraph-attributes node)]
      [(pre)
       (start-new-paragraph)
       (send (current-style-delta) set-delta 'change-family 'modern)
       (list insert-newline)]
      [(div)
       (start-new-paragraph)
       (handle-paragraph-attributes node)]
      [(center)
       (start-new-paragraph)
       (define prev-alignment current-alignment)
       (set! current-alignment 'center)
       (list insert-newline
             (lambda ()
               (set! current-alignment prev-alignment)))]
      [(body)
       (handle-body-attributes node)]
      [(font)
       (handle-font-attributes node)]
      [(big)
       (handle-big)]
      [(small)
       (handle-small)]
      [else
       '()]))
  
  (send canvas set-default-style html-basic-style)
  
  (let loop ([s content])
    (unless (empty? s)
      (define node (car s))
      (cond
        [(sxml:element? node)
         ;(printf "element ~a~n" (sxml:element-name node))
         (case (sxml:element-name node)
           [(br)
            (insert-newline)]
           [(hr)
            (define width-property
              (or (width-attr node)
                  '(width-percent . 100)))
            (define width-pixels
              (if (eq? (car width-property) 'width-pixels)
                  (cdr width-property)
                  ; default width in pixels
                  100))
            (define resizable-property
              (if (eq? (car width-property) 'width-percent)
                  (cons 'resizable (cdr width-property))
                  #f))
            (define align (align-attr node 'center))
            (when (not (last-element-eol?))
              (insert-newline))
            (append-snip (new horz-line-snip% [w width-pixels])
                         #t
                         align
                         (if resizable-property (list resizable-property) '()))
            #;(insert-newline)]
           [(img)
            (handle-img node)]
           [(a)
            (define content (sxml:content node))
            (define href-value (sxml:attr-safer node 'href))
            (define name-value (sxml:attr-safer node 'name))
            (define base-req (send canvas get-current-request))
            (if href-value
                ;; create link
                (cond
                  [(empty? content)
                   #;(printf "unhandled href, no content~n")
                   (append-anchor name-value)]
                  [(non-empty-string? (car content))
                   #;(printf "handle text href ~a~n" content)
                   (define text (car content))
                   (define style-copy (make-object style-delta% 'change-nothing))
                   (send style-copy copy (current-style-delta))
                   (send style-copy set-delta-foreground current-link-color)
                   (send style-copy set-delta 'change-underline #t)
                   (define style (send style-list find-or-create-style (current-style) style-copy))
                   (define link-snip (new html-link-snip% (url href-value) (base-req base-req) (browser-canvas canvas)))
                   (send link-snip set-style style)
                   (send link-snip insert text (string-length text))
                   (append-snip link-snip #f current-alignment (if name-value (list (cons 'anchor name-value)) '()))]
                  [((ntype-names?? '(img)) (car content))
                   #;(printf "handle img href~n")
                   (handle-img (car content) href-value base-req name-value)]
                  [else
                   ;; we don't actually create a link here but we do add the child elements
                   (append-anchor name-value)
                   (loop content)])
                ;; handle non-link content
                (begin
                  (append-anchor name-value)
                  (loop content)))]
           [(ul)
            (define list-table (new table-snip%
                                    (drawing-context (send canvas get-dc))
                                    (defstyle html-basic-style)
                                    (border 0)
                                    (w 1.0)
                                    (rules 'none)
                                    (cellspacing 0)
                                    (cellpadding 0)))
            (parameterize ([current-container list-table]
                           [current-bullet-style (next-bullet-style (current-bullet-style))])
              #;(printf "start unordered list~n")
              (loop (sxml:content node))
              #;(printf "end unordered list~n"))
            (send list-table finalize-table (get-container-width canvas))
            (start-new-paragraph)
            (append-snip list-table
                         #t
                         current-alignment
                         '((resizable . 100)))]
           [(ol)
            (define list-table (new table-snip%
                                    (drawing-context (send canvas get-dc))
                                    (defstyle html-basic-style)
                                    (border 0)
                                    (w 1.0)
                                    (rules 'none)
                                    (cellspacing 0)
                                    (cellpadding 0)))
            (parameterize ([current-container list-table]
                           [current-bullet-style #f]
                           [current-ol-number 0])
              #;(printf "start ordered list~n")
              (loop (sxml:content node))
              #;(printf "end ordered list~n"))
            (send list-table finalize-table (get-container-width canvas))
            (start-new-paragraph)
            (append-snip list-table
                         #t
                         current-alignment
                         '((resizable . 100)))]
           [(li)
            #;(printf "start list item~n")
            (define style (send style-list find-or-create-style (current-style) (current-style-delta)))
            (send (current-container) start-row)
            ;; first column indents list item and has a bullet or number
            (send (current-container) start-cell #:width '(width-pixels . 30) #:valign 'top)
            (cond
              [(current-bullet-style)
               (append-snip (new ul-bullet-snip% (style (current-bullet-style))) #f 'right)]
              [else
               (current-ol-number (add1 (current-ol-number)))
               (append-string (format "~a. " (current-ol-number)) #f #f 'right)])
            (send (current-container) end-cell)
            ;; cell for list item contents
            (send (current-container) start-cell)
            (define prev-alignment current-alignment)
            (set! current-alignment 'unaligned)
            (loop (sxml:content node))
            (set! current-alignment prev-alignment)
            (send (current-container) end-cell)
            (send (current-container) end-row)
            #;(printf "end list item~n")]
           [(table)
            (define border (or (attr->number node 'border) 0))
            (define cellspacing (or (attr->number node 'cellspacing) 2))
            (define cellpadding (or (attr->number node 'cellpadding) 1))
            (define rules (rules-attr node (if (= border 0) 'none 'all)))
            (define width (width-attr node))
            (define width-pixels (and width (eq? (car width) 'width-pixels) (cdr width)))
            (define width-percent (and width (eq? (car width) 'width-percent) (/ (cdr width) 100)))
            (define resizable-property
              (if width-pixels
                  #f
                  (cons 'resizable 100)))
            (define table-snip (new table-snip%
                                    (drawing-context (send canvas get-dc))
                                    (defstyle html-basic-style)
                                    (border border)
                                    (cellspacing cellspacing)
                                    (cellpadding cellpadding)
                                    (rules rules)
                                    (w (or width-pixels width-percent))))
            (parameterize ([current-container table-snip])
              #;(printf "start table~n")
              (loop (sxml:content node))
              #;(printf "end table~n"))
            (send table-snip finalize-table (get-container-width canvas))
            (start-new-paragraph)
            (append-snip table-snip
                         #t
                         current-alignment
                         (if resizable-property
                             (list resizable-property)
                             '()))]
           [(tr)
            #;(printf "start table row~n")
            (send (current-container) start-row)
            (loop (sxml:content node))
            (send (current-container) end-row)
            #;(printf "end table row~n")]
           [(td th)
            #;(printf "start table cell~n")
            (define colspan (or (attr->number node 'colspan) 1))
            (define valign (valign-attr node 'middle))
            (define bgcolor (if (sxml:attr-safer node 'bgcolor)
                                (parse-color (sxml:attr-safer node 'bgcolor))
                                #f))
            (define width (width-attr node))
            (define prev-alignment current-alignment)
            ; todo: use value from table, row, or column as default value if present
            (set! current-alignment
                  (align-attr node
                              (if (eq? (sxml:element-name node) 'th)
                                  'center
                                  'unaligned)))
            (send (current-container) start-cell #:colspan colspan #:valign valign #:bgcolor bgcolor #:width width)
            ; style could be changed by cell's contents
            (define style-copy (make-object style-delta% 'change-nothing))
            (send style-copy copy (current-style-delta))
            (parameterize ([current-style-delta style-copy])
              (when bgcolor
                (send (current-style-delta) set-delta-background bgcolor))
              (loop (sxml:content node)))
            (send (current-container) end-cell)
            (set! current-alignment prev-alignment)
            #;(printf "end table cell~n")]
           [(script style meta link applet form isindex)
            ;; skip element
            void]
           [else
            (define style-copy (make-object style-delta% 'change-nothing))
            (send style-copy copy (current-style-delta))
            (parameterize ([current-style-delta style-copy]
                           [current-block (update-block (current-block) (car node))])
              ;; handle the element. returns a list of functions to call when closing the tag
              ;; will also update the current style
              (define close-tag-funcs (handle-element node))

              ;(eprintf "handling ~a~n" (car node))

              ;; recurse into the element
              (loop (cdr node))
              
              ;; perform actions to close the tag
              (for ([f (in-list close-tag-funcs)])
                (f)))
            ;(eprintf "ending ~a~n" (car node))

            ;; insert a newline after a "paragraph" element unless the next element is a <br> or <hr>
            ;; don't do this for 'center' elements
            (when (is-paragraph-element? (car node))
              (when (and (not (empty? (cdr s)))
                         (not (followed-by-newline? (cadr s))))
                #;(eprintf "inserting newline after 'paragraph'~n")
                (insert-newline)))])
         (loop (cdr s))]
        [(string? node)
         (case (current-block)
           [(head title)
            void]
           [(pre)
            (define style (send style-list find-or-create-style (current-style) (current-style-delta)))
            (append-string node style (string-suffix? node "\n"))
            #;(eprintf "pre: ~aEND~n" node)]
           [else
            ;; attempt to normalize spacing when one string ends with whitespace and the following
            ;; string starts with whitespace. skip strings that are whitespace only.
            (define text (if (regexp-match #px"^\\s+$" node)
                             ""
                             (string-trim node #:right? #f #:left? (last-element-ews?))))
            (when (non-empty-string? text)
              (define style (send style-list find-or-create-style (current-style) (current-style-delta)))
              (append-string text style #f current-alignment)
              #;(eprintf "~a,~a paragraph: ~a~n" (current-block) current-alignment text))])
         (loop (cdr s))]
        #;[(attr-list? node)
           (printf "atrributes ~a~n" (cdr node))
           (loop (cdr s))]
        [else
         ;(printf "skip ~a~n" node)
         (loop (cdr s))])))
    
    void)

(define (render-html-to-text port canvas [img-ok? #f] [eval-ok? #f])
  (unless (input-port? port)
    (raise-type-error 'render-html-to-text "input port" 0 (list port canvas)))
  ;; TEMP - enable auto wrap automatically for html pages
  (send canvas set-mode 'layout)
  ;; set the canvas background color to match the default for html
  (send canvas set-canvas-background html-text-bg-color)
  (convert-html port canvas img-ok?))
