#lang racket/gui

(require html
         xml
         net/url)

;; use browser/htmltext frome drracket as a starting point and basis for the api
;; 

;(require "bullet.rkt")
;(require "entity-names.rkt")
;(require "option-snip.rkt")

(provide render-html-to-text)

(define (fixup-newlines c)
  (cond
    [(string? c)
     (string-replace c "\r\n" "\n")]
    [(pair? c)
     (cons (fixup-newlines (car c))
           (fixup-newlines (cdr c)))]
    [else
     c]))

(define (read-html a-port)
  (let* ([xml (parameterize ([read-html-comments #t]
                             [use-html-spec #f])
                (read-html-as-xml a-port))]
         [xexpr (map xml->xexpr xml)])
    (car xexpr)))

(define (parse-html a-port)
  (let ([raw (read-html a-port)])
    (fixup-newlines raw)))

(define (convert-html a-port a-text)
  (define content (parse-html a-port))
  content)

(define (render-html-to-text port text%-obj img-ok? eval-ok?)
  (unless (input-port? port)
    (raise-type-error 'render-html-to-text "input port" 0 (list port text%-obj)))
  (dynamic-wind
    ;(lambda () (send text%-obj begin-edit-sequence #f))
    (lambda () (convert-html port text%-obj))
    ;(lambda () (send text%-obj end-edit-sequence))))
    ))
