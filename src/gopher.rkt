#lang racket/base

(require racket/string)
(require racket/port)
(require racket/tcp)

(provide gopher-fetch
         parse-dir-entity
         dir-entity->url
         menu-line-split
         (struct-out gopher-response)
         (struct-out gopher-dir-entity))

(struct gopher-response
  (item-type
   error?
   data-port)
  #:prefab)

(struct gopher-dir-entity
  (type        ; character
   user-name   ; string
   selector    ; string
   host        ; string
   port)       ; string
  #:prefab)

(define (parse-dir-entity s)
  ;(eprintf "parse-dir-entity ~a~n" s)
  (define type (string-ref s 0))
  (define fields (menu-line-split s))
  (cond
    [(>= (length fields) 4)
     (gopher-dir-entity type
                        (car fields)
                        (cadr fields)
                        (caddr fields)
                        (cadddr fields))]
    [(or (equal? type #\i)
         (equal? type #\3))
     ;; be lenient with info and error types that are missing fields since only the user-name
     ;; is actually needed
     (gopher-dir-entity type
                        (car fields)
                        ""
                        ""
                        "")]
    [else
     (gopher-dir-entity #\3
                        "malformed dir entity"
                        ""
                        "error.host"
                        1)]))

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

(define (menu-line-split s)
  (define len (string-length s))
  (let loop-mls ([cursor 1]
                 [end 1])
    (cond
      [(>= end len)
       (list (substring s cursor end))]
      [(eq? (string-ref s end) #\tab)
       (cons (substring s cursor end)
             (loop-mls (add1 end) (add1 end)))]
      [else (loop-mls cursor (add1 end))])))

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
  (define-values (data-port error?) (gopher-fetch-data host selector port))
  (gopher-response type error? data-port))

(define (gopher-fetch-data host selector port)
  (define (send-selector sel-string out)
    (write-bytes (bytes-append
                  (string->bytes/latin-1 sel-string)
                  #"\r\n")
                 out)
    (flush-output out))

  #;(eprintf "gopher selector: ~a~n" selector)

  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (values (open-input-string (format "error: ~a" (exn-message exn)))
                             #t))])
    (define-values (in out) (tcp-connect host port))
    (send-selector selector out)
    (tcp-abandon-port out)
    (values in #f)))
