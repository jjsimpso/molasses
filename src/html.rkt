#lang racket/gui

#;(require (except-in html make-object)
           xml
           net/url)

(require html-parsing
         (except-in html make-object)
         xml
         sxml
         net/url)

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

(provide render-html-to-text)

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
  (define new-bitmap (make-object bitmap%
                                (gopher-response-data-port response)
                                'unknown))
  (close-input-port (gopher-response-data-port response))
  new-bitmap)

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
  (string-normalize-spaces (string-join (takef c string?))))

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
    [else
     (define named-color (send the-color-database find-color c-string))
     (if named-color
         named-color
         (make-color 16 16 16))]))

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
  (define current-container (make-parameter #f))
  ;; alignment is a special case since it isn't controlled by a style and must be applied to each paragraph
  (define current-alignment 'unaligned)  
  ;; don't use a parameter for link color since it will change so rarely
  (define current-link-color html-link-color)
  (define current-vlink-color html-vlink-color)
  (define current-font-size 3)

  (define horz-inset (send canvas horizontal-inset))

  ;; need to abstract functions which add elements in order to handle tables
  (define (append-string s . rest-args)
    (if (current-container)
        (send/apply (current-container) append-string s `(,@rest-args))
        (send/apply canvas append-string s `(,@rest-args))))

  (define (append-snip s . rest-args)
    (if (current-container)
        (send/apply (current-container) append-snip s `(,@rest-args))
        (send/apply canvas append-snip s `(,@rest-args))))
  
  (define (last-element-eol?)
    (send canvas last-element-eol?))
  
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
    ;(eprintf "inserting newline~n")
    (append-string "\n" #f #t))

  (define (start-new-paragraph)
    (when (not (last-element-eol?))
      (eprintf "starting new paragraph~n")
      (insert-newline)))

  (define (last-node-in-paragraph? s)
    (and (is-paragraph-element? (current-block))
         (empty? (cdr s))))
  
  (define (is-block-element? elem)
    ;; treat head, title, and body as blocks even though they aren't technically block elements
    ;; head and title need special treatment but body currently doesn't
    (define block-elements '(head title body h1 h2 h3 h4 h5 h6 p pre center))
    (memq elem block-elements))

  ;; defines elements that create paragraphs and require a newline or two after closing
  ;; may eventually get rid of this function
  (define (is-paragraph-element? elem)
    (define para-elements '(h1 h2 h3 h4 h5 h6 p pre center))
    (memq elem para-elements))
  
  (define (update-block current-block elem)
    (if (is-block-element? elem)
        elem
        current-block))
  
  (define (handle-body-attributes node)
    (for/list ([attr (in-list (sxml:attr-list node))])
      (eprintf "handling body attribute ~a~n" attr)
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
         (eprintf "  null func for attribute ~a~n" attr)
         (lambda () void)])))
  
  (define (handle-font-attributes node)
    (define font-size-vec #(7/12 5/6 1.0 1.125 1.5 2.0 3.0))  ; 7 10 12 13.5 18 24 36
    (define size-value (sxml:attr node 'size))
    (define color-value (sxml:attr node 'color))

    ;; changes current style only, no need for close tag function
    (when color-value
      (eprintf "setting font color to ~a~n" color-value)
      (define text-color (parse-color color-value))
      (send (current-style-delta) set-delta-foreground text-color))
    
    (if size-value
        (let ([prev-size current-font-size]
              [size (parse-font-size size-value current-font-size)])
          (eprintf "new font size: ~a -> ~a -> ~a~n" prev-size size-value size)
          (set! current-font-size size)
          (send (current-style-delta) set-size-mult (vector-ref font-size-vec (sub1 size)))
          (list (lambda ()
                  (set! current-font-size prev-size))))
        '()))
  
  (define (handle-paragraph-attributes node)
    (define value (sxml:attr node 'align))
    (if value
        (let ([prev-alignment current-alignment])
          (set! current-alignment
                (cond
                  [(string-ci=? value "center") 'center]
                  [(string-ci=? value "left") 'left]
                  [(string-ci=? value "right") 'right]
                  [else current-alignment]))
          (list (lambda ()
                  (set! current-alignment prev-alignment))))
        '()))
  
  (define (handle-img node [url #f] [base-url #f])
    (when img-ok?
      (define src-value (sxml:attr node 'src))
      (define request (send canvas get-current-request))
      (when request
        (define align
          (case (and (sxml:attr node 'align)
                     (string-downcase (sxml:attr node 'align)))
            [("left") 'left]
            [("right") 'right]
            [("center") 'center]
            [else current-alignment]))
        (define bm (load-new-bitmap src-value request))
        (define snip (if url
                         (new html-link-img-snip% (url url) (base-url base-url) (browser-canvas canvas))
                         (make-object image-snip%)))
        (send snip set-bitmap bm)
        (append-snip snip #f align))))
  
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
       '()]
      [(center)
       (start-new-paragraph)
       (define prev-alignment current-alignment)
       (set! current-alignment 'center)
       (list (lambda ()
               (set! current-alignment prev-alignment)))]
      [(body)
       (handle-body-attributes node)]
      [(font)
       (handle-font-attributes node)]
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
            (define width-value (sxml:attr node 'width))
            (define width-property
              (if width-value
                  (if (string-contains? width-value "%")
                      (cons 'width-percent
                            (string->number (car (string-split width-value "%"))))
                      (cons 'width-pixels
                            (string->number width-value)))
                  '(width-percent . 100)))
            (define align
              (case (and (sxml:attr node 'align)
                         (string-downcase (sxml:attr node 'align)))
                [("left") 'left]
                [("right") 'right]
                [("center") 'center]
                [else current-alignment]))
            (when (not (last-element-eol?))
              (insert-newline))
            (append-snip (new horz-line-snip%)
                         #t
                         align
                         (list width-property))
            #;(insert-newline)]
           [(img)
            (handle-img node)]
           [(a)
            (define content (sxml:content node))
            (define href-value (sxml:attr node 'href))
            (define base-url (request->url (send canvas get-current-request)))
            (when (not (empty? content))
              (cond
                [(non-empty-string? (car content))
                 (printf "handle href ~a~n" content)
                 (define text (car content))
                 (define style-copy (make-object style-delta% 'change-nothing))
                 (send style-copy copy (current-style-delta))
                 (send style-copy set-delta-foreground current-link-color)
                 (define style (send style-list find-or-create-style (current-style) style-copy))
                 (define link-snip (new html-link-snip% (url href-value) (base-url base-url) (browser-canvas canvas)))
                 (send link-snip set-style style)
                 (send link-snip insert text (string-length text))
                 (append-snip link-snip #f current-alignment)]
                [((ntype-names?? '(img)) (car content))
                 (printf "handle img href~n")
                 (handle-img (car content) href-value base-url)]
                [else
                 (printf "unhandled href~n")]))]
           [(table)
            (define table-snip (new table-snip%
                                    (drawing-context (send canvas get-dc))
                                    (defstyle html-basic-style)))
            (parameterize ([current-container table-snip])
              (printf "start table~n")
              (loop (sxml:content node))
              ;; todo: getting size from the canvas won't work for nested tables
              (define-values (dw dh) (send canvas get-drawable-size))
              (send (current-container) finalize-table dw)
              (printf "end table~n"))
            (append-snip table-snip #t current-alignment)]
           [(th)
            (printf "table header~n")]
           [(tr)
            (printf "start table row~n")
            (send (current-container) start-row)
            (loop (sxml:content node))
            (send (current-container) end-row)
            (printf "end table row~n")]
           [(td)
            (printf "start table cell~n")
            (send (current-container) start-cell)
            (loop (sxml:content node))
            (send (current-container) end-cell)
            (printf "end table cell~n")]
           [else
            (define style-copy (make-object style-delta% 'change-nothing))
            (send style-copy copy (current-style-delta))
            (parameterize ([current-style-delta style-copy]
                           [current-block (update-block (current-block) (car node))])
              ;; handle the element. returns a list of functions to call when closing the tag
              ;; will also update the current style
              (define close-tag-funcs (handle-element node))
              
              ;; recurse into the element
              (loop (cdr node))
              
              ;; perform actions to close the tag
              (for ([f (in-list close-tag-funcs)])
                (f)))
            ;(eprintf "ending ~a~n" (car node))

            ;; insert a newline after a "paragraph" element unless the next element is a <br> or <hr>
            ;; don't do this for 'center' elements
            (when (is-paragraph-element? (car node))
              (insert-newline)
              (when (and (not (empty? (cdr s)))
                         (not (eq? (car node) 'center)) ; center elements don't need space below
                         (not (followed-by-newline? (cadr s))))
                (eprintf "inserting newline after 'paragraph'~n")
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
            (define text (string-normalize-spaces node))
            (when (non-empty-string? text)
              (define style (send style-list find-or-create-style (current-style) (current-style-delta)))
              (append-string (string-append text " ") style #f current-alignment)
              #;(eprintf "~a,~a paragraph: ~a~n" (current-block) current-alignment text))
            (when (not (non-empty-string? text))
              (eprintf "skipped newline char~n"))])
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
