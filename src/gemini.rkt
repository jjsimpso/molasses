#lang racket/base

(require racket/string)
(require racket/port)
(require openssl)

(provide gemini-fetch
         (struct-out gemini-response))

(struct gemini-response
  (status
   meta
   data-port
   from-url)
  #:prefab)

(define (parse-header header)
  (let ([parts (string-split header)])
     (values (or (string->number (car parts)) 40)
             (if (> (length parts) 1)
                 (cadr parts)
                 ""))))

(define (make-url-and-query host path-plus-query port)
  (if (string-contains? path-plus-query "?")
      (let ([splits (string-split path-plus-query "?")])
        (values (string-append "gemini://" host ":" (number->string port) (car splits))
                (string-append "?" (cadr splits))))
      (values (string-append "gemini://" host ":" (number->string port) path-plus-query)
              "")))

(define (gemini-fetch host path-plus-query port)
  (let-values ([(in out) (ssl-connect/enable-break
                          host
                          port)])

    (define-values (url query) (make-url-and-query host path-plus-query port))
    (write-bytes (bytes-append (string->bytes/latin-1 url)
                               (string->bytes/latin-1 query)
                               #"\r\n")
                 out)
    (flush-output out)
    (ssl-abandon-port out)

    (let ([header (read-line in)])
      (let-values ([(status meta) (parse-header header)])
        (gemini-response status meta in url)))))
