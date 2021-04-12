#lang racket/base

(require racket/string)
(require racket/port)
(require racket/tcp)

(provide gopher-fetch
         parse-dir-entity
         dir-entity->url
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
