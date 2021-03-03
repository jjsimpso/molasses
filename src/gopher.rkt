#lang racket/base

(require racket/string)
(require racket/port)
(require racket/tcp)
(require net/url-string)

(provide fetch
         (struct-out response))

(struct response
  (item-type
   error?
   data)
  #:prefab)

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

(define (url-path->selector pp-list)
  (if (null? pp-list)
      "."
      (string-join
       (map path/param-path pp-list)
       "/")))

(define (fetch url-string [type #f])
  (define url-struct (string->url url-string))
  (define scheme (url-scheme url-struct))
  (define host (url-host url-struct))
  (define path (url-path url-struct))
  (define port (url-port url-struct))

  (eprintf "fetching: ~a, ~a, ~a, ~a~n" scheme host path port)
  (cond
    [(equal? scheme "gopher")
     (gopher-fetch host
                   path
                   type
                   (or port 70))]
    [else #f]))

(define (gopher-fetch host path-list type port)
  (define item-type-from-url
    (if (and (not type)
             (not (null? path-list))
             (gopher-item-type? (path/param-path (car path-list))))
        (path/param-path (car path-list))
        #f))
  (define selector
    (if item-type-from-url
        (url-path->selector (cdr path-list))
        (url-path->selector path-list)))
  
  (define-values (data error?) (gopher-fetch-data host selector port))

  (response (or type item-type-from-url)
            error?
            data))

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
