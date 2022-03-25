#lang racket/gui

#;(require (except-in html make-object)
           xml
           net/url)

(require html-parsing
         sxml
         net/url)

;; use browser/htmltext frome drracket as a starting point and basis for the api
;; 

;(require "bullet.rkt")
;(require "entity-names.rkt")
;(require "option-snip.rkt")

(provide render-html-to-text)

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

    (define (handle-element elem)
      (case elem
        [(b)
         (eprintf "applying bold style at ~a~n" (current-pos))
         (send a-text change-style delta:bold (current-pos))]
        [(h1)
         (eprintf "applying h1 style at ~a~n" (current-pos))
         (send a-text change-style delta:h1 (current-pos))]
        [else
         void]))
    

    (define (update-style-delta elem)
      (case elem
        [(b)
         ;(eprintf "applying bold style at ~a~n" (current-pos))
         (send (current-style-delta) set-delta 'change-bold)]
        [(h1)
         ;(eprintf "applying h1 style at ~a~n" (current-pos))
         (send (current-style-delta) set-delta 'change-bold)
         (send (current-style-delta) set-size-mult 2.0)]
        [else
         void]))

    (define (handle-attribute attr)
      (case (car attr)
        [(align)
         (define value (cadr attr))
         (define alignment
           (cond
             [(string-ci=? value "center") 'center]
             [(string-ci=? value "left") 'left]
             [(string-ci=? value "right") 'right]))
         (define start-paragraph (send a-text position-paragraph (current-pos)))
         (lambda ()
           ;(eprintf "closing align tag~n")
           (for ([i (in-range start-paragraph (add1 (send a-text position-paragraph (current-pos))))])
             (eprintf "*** align ~a paragraph ~a~n" alignment i)
             (send a-text set-paragraph-alignment i alignment)))]
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
           (parameterize ([current-style-delta style-copy])
             (update-style-delta (car node))
             (eprintf "setting style to ~a,~a~n"
                      (send (current-style-delta) get-weight-on)
                      (send (current-style-delta) get-size-mult))
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
           
           (eprintf "changing style back to ~a,~a~n"
                    (send (current-style-delta) get-weight-on)
                    (send (current-style-delta) get-size-mult))
           (change-style html-basic-style (current-pos))
           (change-style (current-style-delta) (current-pos))
           (loop (cdr s))]
          [(string? node)
           (a-text-insert node)
           (eprintf "paragraph ~a: ~a~n" (send a-text position-paragraph (current-pos)) node)
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
  (convert-html port text%-obj))
