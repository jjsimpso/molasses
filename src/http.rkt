#lang racket

(require net/url)
(require net/http-client)

(provide http-fetch
         (struct-out http-response))

(struct http-response
  (error?
   status
   data-port)
  #:prefab)

(define (http-fetch host path port)
  (define url (string->url path))
  (define-values (status headers resp-port) (http-sendrecv host (url->string url) #:port port))
  (http-response (not (string-contains? (bytes->string/latin-1 status) "200 OK")) status resp-port))
