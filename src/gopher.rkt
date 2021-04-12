#lang racket/base

(require racket/string)
(require racket/port)
(require racket/tcp)
(require net/url-string)

(provide gopher-fetch
         parse-dir-entity
         dir-entity->url
         url->url-with-type
         (struct-out gopher-response)
         (struct-out gopher-dir-entity))

(struct gopher-response
  (item-type
   error?
   data)
  #:prefab)

(struct gopher-dir-entity
  (type        ; character
   user-name   ; string
   selector    ; string
   host        ; string
   port)       ; string
  #:prefab)

(define (parse-dir-entity s)
  (define type (string-ref s 0))
  (define fields (string-split (substring s 1) "\t" #:trim? #f))
  (gopher-dir-entity type
                   (car fields)
                   (cadr fields)
                   (caddr fields)
                   (cadddr fields)))


(define (dir-entity->url s)
  (string-append "gopher://"
                 (gopher-dir-entity-host s) ":"
                 (gopher-dir-entity-port s)
                 (gopher-dir-entity-selector s)))

(define (dir-entity->url-with-type s)
  (string-append "gopher://"
                 (gopher-dir-entity-host s) ":"
                 (gopher-dir-entity-port s)
                 "/" (string (gopher-dir-entity-type s))
                 (gopher-dir-entity-selector s)))

(define (gopher-item-type? type-string)
  (or (string=? type-string "0")
      (string=? type-string "1")
      (string=? type-string "2")
      (string=? type-string "3")
      (string=? type-string "4")
      (string=? type-string "5")
      (string=? type-string "6")
      (string=? type-string "7")
      (string=? type-string "8")
      (string=? type-string "9")
      (string=? type-string "T")
      (string=? type-string "g")
      (string=? type-string "I")
      ;; unofficial
      (string=? type-string "h")))

(define (url->url-with-type url [type #f])
  (define url-struct (string->url url))
  (define path-list (url-path url-struct))

  ;; if no scheme is specified in the url string, default to gopher
  ;; string->url doesn't parse correctly without a scheme so re-run it
  #;(when (not (url-scheme url-struct))
    (set! url-struct (string->url (string-append "gopher://" url)))
    (set! path-list (url-path url-struct)))
      
  (if (and (not (null? path-list))
           (gopher-item-type? (path/param-path (car path-list))))
      ; url has a type, just return it
      url
      (string-append (if (url-scheme url-struct) (url-scheme url-struct) "gopher") "://"
                     (url-host url-struct)
                     (if (and (url-port url-struct)
                              (not (= (url-port url-struct) 70)))
                         (string-append ":" (number->string (url-port url-struct)))
                         "")
                     (if type (string-append "/" (string type) "/") "")
                     (string-join (map path/param-path path-list) "/"))))

(define (url-path->selector pp-list)
  (if (null? pp-list)
      "/" ; CRLF may be a better choice but / appears to work for modern servers
      (string-join
       (map path/param-path pp-list)
       "/")))

#;(define (fetch/from-url url-string [type #f])
  (define url-struct (string->url url-string))
  (define scheme (url-scheme url-struct))
  (define host (url-host url-struct))
  (define path (url-path url-struct))
  (define port (url-port url-struct))

  (eprintf "fetching: ~a, ~a, ~a, ~a~n" scheme host path port)
  (cond
    [(or (equal? scheme "gopher")
         (not scheme)) ; default to gopher if not specified
     (gopher-fetch/from-url host
                            path
                            type
                            (or port 70))]
    [else #f]))

;; this version of gopher-fetch takes a path/param list as the selector
#;(define (gopher-fetch/from-url host path-list type port)
  (define item-type-from-url
    (if (and (not type)
             (not (null? path-list))
             (gopher-item-type? (path/param-path (car path-list))))
        ; return a character for the item type
        (string-ref (path/param-path (car path-list)) 0)
        #f))
  (define selector
    (if item-type-from-url
        (url-path->selector (cdr path-list))
        (url-path->selector path-list)))
  
  (define-values (data error?) (gopher-fetch-data host selector port))

  (gopher-response (or type item-type-from-url)
            error?
            data))

(define (gopher-fetch host selector type port)
  (define-values (data error?) (gopher-fetch-data host selector port))
  (gopher-response type error? data))

(define (gopher-fetch-data host selector port)
  (define (send-selector sel-string out)
    (write-bytes (bytes-append
                  (string->bytes/latin-1 sel-string)
                  #"\r\n")
                 out)
    (flush-output out))

  (eprintf "gopher selector: ~a~n" selector)

  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (values (format "error: ~a" (exn-message exn))
                             #t))])
    (define-values (in out) (tcp-connect host port))
    (send-selector selector out)
    (define data (port->bytes in))
    (eprintf "closing ports~n")
    (close-output-port out)
    (close-input-port in)
    ;; return data read from port
    (values data #f)))
