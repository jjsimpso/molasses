#lang racket/gui

#;(require (except-in html make-object)
           xml
           net/url)

(require html-parsing
         sxml
         net/url)

(require "config.rkt")

;; use browser/htmltext frome drracket as a starting point and basis for the api
;; 

;(require "bullet.rkt")
;(require "entity-names.rkt")
;(require "option-snip.rkt")

(provide render-html-to-text)

(define paragraph-elements '(h1 h2 h3 h4 p))

(define delta:fixed (make-object style-delta% 'change-family 'modern))
(define delta:default-face (make-object style-delta% 'change-family 'default))
(define delta:bold (make-object style-delta% 'change-bold))
(define delta:underline (make-object style-delta% 'change-underline #t))
(define delta:italic (make-object style-delta% 'change-italic))
(define delta:h1
  (let ([d (make-object style-delta% 'change-bold)])
    (send d set-size-mult 2.0)
    d))
(define delta:h2
  (let ([d (make-object style-delta% 'change-bold)])
    (send d set-size-mult 1.5)
    d))
(define delta:h3
  (let ([d (make-object style-delta% 'change-bold)])
    (send d set-size-mult 1.2)
    d))
(define delta:h4 (make-object style-delta% 'change-bold))
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

(define delta:center (make-object style-delta% 'change-alignment 'center))
(define delta:symbol (make-object style-delta% 'change-family 'symbol))

(define (fixup-newlines c)
  (cond
    [(string? c)
      (string-replace c "\r\n" "\n")]
    [(pair? c)
     (cons (fixup-newlines (car c))
           (fixup-newlines (cdr c)))]
    [else
     c]))

#;(define (read-html a-port)
  (let* ([xml (parameterize ([read-html-comments #t]
                             [use-html-spec #f])
                (read-html-as-xml a-port))]
         [xexpr (map xml->xexpr xml)])
    (car xexpr)))

(define (parse-color c-string)
  (if (and (string? c-string)
           (equal? (string-ref c-string 0) #\#))
      (make-color (string->number (substring c-string 1 3) 16)
                  (string->number (substring c-string 3 5) 16)
                  (string->number (substring c-string 5 7) 16))
      (make-color 255 255 255)))

(define (read-html a-port)
  (html->xexp a-port))

(define (parse-html a-port)
  (let ([raw (read-html a-port)])
    (fixup-newlines raw)))

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

(define (convert-html a-port a-text)
  (define content (parse-html a-port))
  (define html-basic-style
    (let ([sl (send a-text get-style-list)])
      (or (send sl find-named-style "Html Standard")
          (send sl find-named-style "Standard")
          (send sl find-named-style "Basic"))))
  (define current-style-delta (make-parameter (make-object style-delta% 'change-nothing)))
  (define current-element (make-parameter #f))
  ;; alignment is a special case since it isn't controlled by a style and must be applied to each paragraph
  (define current-alignment 'left)  
  ;; don't use a parameter for link color since it will change so rarely
  (define current-link-color html-link-color)
  (define current-vlink-color html-vlink-color)

  (with-method ([a-text-insert (a-text insert)]
                [current-pos (a-text last-position)]
                [delete (a-text delete)]
                [get-character (a-text get-character)]
                [change-style (a-text change-style)])

    (define (insert what) 
      (let ([pos-before (current-pos)])
        (a-text-insert what pos-before)
        (let ([pos-after (current-pos)])
          (change-style html-basic-style pos-before pos-after))))

    (define (insert-newline)
      (let ((s (make-object string-snip% "\n")))
        (send s set-flags (cons 'hard-newline (send s get-flags)))
        (send a-text insert s)))
    
    (define (set-alignment align)
      ;(eprintf "setting alignment to ~a~n" align)
      (send a-text set-paragraph-alignment
            (send a-text position-paragraph (current-pos))
            align))
    
    (define (update-style-delta elem)
      (case elem
        [(a)
         (send (current-style-delta) set-delta-foreground current-link-color)]
        [(b)
         ;(eprintf "applying bold style at ~a~n" (current-pos))
         (send (current-style-delta) set-delta 'change-bold)]
        [(h1)
         ;(eprintf "applying h1 style at ~a~n" (current-pos))
         (send (current-style-delta) set-delta 'change-bold)
         (send (current-style-delta) set-size-mult 2.0)]
        [(h2)
         (send (current-style-delta) set-delta 'change-bold)
         (send (current-style-delta) set-size-mult 1.5)]
        [(h3)
         (send (current-style-delta) set-delta 'change-bold)
         (send (current-style-delta) set-size-mult 1.2)]
        [(h4)
         (send (current-style-delta) set-delta 'change-bold)]
        [(pre)
         (send (current-style-delta) set-delta 'change-family 'modern)]
        [else
         void]))

    (define (handle-attribute attr)
      (case (car attr)
        [(align)
         (define value (cadr attr))
         (define prev-alignment current-alignment)
         (set! current-alignment
           (cond
             [(string-ci=? value "center") 'center]
             [(string-ci=? value "left") 'left]
             [(string-ci=? value "right") 'right]))
         (lambda ()
           (set! current-alignment prev-alignment))]
        [(bgcolor)
         (define bg-color (parse-color (cadr attr)))
         (send (send a-text get-canvas) set-canvas-background bg-color)
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
        [(href)
         (define link-start-pos (current-pos))
         (define vlink-delta (make-object style-delta%))
         (send vlink-delta set-delta-foreground current-vlink-color)
         (lambda ()
           ;; add clickback to link region (temp function to just change the link color)
           (send a-text set-clickback
                 link-start-pos
                 (current-pos)
                 (lambda (text-widget start end)
                   (eprintf "link to ~a clicked~n" (cadr attr))
                   (send text-widget change-style vlink-delta start end))))]
        [else
         (lambda () void)]))
    
    (send a-text set-styles-sticky #t)
    (eprintf "sticky = ~a~n" (send a-text get-styles-sticky))
    (send a-text change-style html-basic-style)
    
    (let loop ([s content])
      (unless (empty? s)
        (define node (car s))
        (cond
          [(sxml:element? node)
           (printf "element ~a~n" (sxml:element-name node))
           (define style-copy (make-object style-delta% 'change-nothing))
           (send style-copy copy (current-style-delta))
           (parameterize ([current-style-delta style-copy]
                          [current-element (car node)])
             (update-style-delta (car node))
             (change-style (current-style-delta) (current-pos))
             ;; get attributes for this tag and process them
             ;; returns a list of functions to call when closing the tag
             (define close-tag-funcs
               (for/list ([attr (in-list (sxml:attr-list node))])
                 (eprintf "handling attribute ~a~n" attr)
                 (handle-attribute attr)))
             ;; recurse into the element
             (loop (cdr node))
             ;; perform actions to close the tag
             (for ([f (in-list close-tag-funcs)])
               (f)))

           (eprintf "ending ~a~n" (car node))
           (when (memq (car node) paragraph-elements)
             (eprintf "inserting newlines~n")
             (insert-newline)
             (insert-newline))
           (change-style html-basic-style (current-pos))
           (change-style (current-style-delta) (current-pos))
           (loop (cdr s))]
          [(string? node)
           (case (current-element)
             [(head title)
              void]
             [(pre)
              (define insert-pos (current-pos))
              (a-text-insert node)
              (send a-text set-paragraph-alignment (send a-text position-paragraph insert-pos) 'left)]
             [else
              (define text (string-trim node #px"[\n\r]+" #:left? #f))
              (when (non-empty-string? text)
                (define insert-pos (current-pos))
                ;; special case for paragraphs with multiple strings (which means that newlines were found when parsing)
                ;; add space to the end of the line so strings will flow together, essentially replacing the trimmed
                ;; newline with a space
                (if (and (eq? (current-element) 'p)
                         (and (not (empty? (cdr s))) (string? (cadr s))))
                    (a-text-insert (string-append text " "))
                    (a-text-insert text))
                (send a-text set-paragraph-alignment (send a-text position-paragraph insert-pos) current-alignment)
                (eprintf "~a,~a paragraph ~a: ~a~n" (current-element) current-alignment (send a-text position-paragraph insert-pos) node))])
           (loop (cdr s))]
          #;[(attr-list? node)
           (printf "atrributes ~a~n" (cdr node))
           (loop (cdr s))]
          [else
           ;(printf "skip ~a~n" node)
           (loop (cdr s))])))
    
    void))

(define (render-html-to-text port text%-obj img-ok? eval-ok?)
  (unless (input-port? port)
    (raise-type-error 'render-html-to-text "input port" 0 (list port text%-obj)))
  ;; TEMP - enable auto wrap automatically for html pages
  (send text%-obj auto-wrap #t)
  ;; set the canvas background color to match the default for html
  (define canvas (send text%-obj get-active-canvas))
  (when canvas
    (send canvas set-canvas-background html-text-bg-color)) 
  (convert-html port text%-obj))
