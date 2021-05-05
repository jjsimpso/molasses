#lang racket

(require "gopher.rkt")

(provide url->request
         request->url
         dir-entity->request
         (struct-out request))

(struct request
  (protocol       ; symbol
   host           ; string
   port           ; port-number?
   path/selector  ; string
   type)          ; character
  #:prefab)

(define (dir-entity->request dir-entity)
  (request 'gopher
           (gopher-dir-entity-host dir-entity)
           (string->number (gopher-dir-entity-port dir-entity))
           (gopher-dir-entity-selector dir-entity)
           (gopher-dir-entity-type dir-entity)))

;; (regexp-match #px"^(\\w+://)?([a-zA-Z0-9\\.]+)(:\\d+)?(/.*)?" "gopher://abc6.def.com:70/a/b/c.txt")
(define (url->request url)
  (define (string->protocol s)
    (cond
      [(false? s) 'gopher] ;; default to gopher
      [(equal? s "gopher://") 'gopher]
      [else 'unsupported]))

  (define (default-port p)
    (cond
      [(equal? p 'gopher) 70]
      [(equal? p 'gemini) 1965]
      [(equal? p 'http) 80]
      [else 70]))

  (define (strip-type-from-path path protocol)
    (if (and (equal? protocol 'gopher)
             (> (string-length path) 2)
             (and (equal? (string-ref path 0) #\/)
                  (equal? (string-ref path 2) #\/)))
        (substring path 2)
        path))

  (define (get-type-from-path path protocol)
    (if (and (equal? protocol 'gopher)
             (> (string-length path) 2)
             (and (equal? (string-ref path 0) #\/)
                  (equal? (string-ref path 2) #\/)))
        (string-ref path 1)
        #\1))  ; default to menu type
  
  (define url-components (regexp-match #px"^(\\w+://)?([a-zA-Z0-9\\.]+)(:\\d+)?(/.*)?$" url))

  (if url-components
      (let ([protocol (string->protocol (second url-components))]
            [host (third url-components)]
            [port (fourth url-components)]
            [path/selector (or (fifth url-components) "")])
        (request protocol
                 host
                 (or (and (string? port) (string->number (substring port 1)))
                     (default-port protocol))
                 (strip-type-from-path path/selector protocol)
                 (get-type-from-path path/selector protocol)))
      #f))

(define (request->url req)
  (define (protocol->string p)
    (cond
      [(equal? p 'gopher) "gopher://"]
      [(equal? p 'gemini) "gemini://"]
      [(equal? p 'http)   "http://"]
      [else "gopher://"]))

  (define (port-number->string port protocol)
    (cond
      [(equal? protocol 'gopher)
       (if (= port 70)
           ""
           (string-append ":" (number->string port)))]
      [(equal? protocol 'gemini)
       (if (= port 1965)
           ""
           (string-append ":" (number->string port)))]
      [(equal? protocol 'http)
       (if (= port 80)
           ""
           (string-append ":" (number->string port)))]))
  
  (string-append (protocol->string (request-protocol req))
                 (request-host req)
                 (port-number->string (request-port req) (request-protocol req))
                 ;; follow the convention of displaying the type in the URL. if selector is null add a '/' after the type
                 (if (request-type req)
                     (if (and (request-path/selector req) (not (equal? (request-path/selector req) "")))
                         (format "/~a" (request-type req))
                         (format "/~a/" (request-type req)))
                     "")
                 (request-path/selector req)))
