#lang racket

(require "gopher.rkt")

(provide url->request
         request->url
         dir-entity->request
         gopher-url-request?
         gopher-url-request->url
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
(define (url->request url #:default-scheme [default-scheme 'gopher])
  (define url-components (regexp-match #px"^(\\w+://)?([a-zA-Z0-9\\-\\.]+)(:\\d+)?(/.*)?$" url))

  (if url-components
      (let ([protocol (string->protocol (second url-components) default-scheme)]
            [host (third url-components)]
            [port (fourth url-components)]
            [path/selector (or (fifth url-components) "")])
        ;(printf "url->request: ~a,~a,~a,~a~n" protocol host port path/selector)
        (cond
          [(eq? protocol 'gemini)
           (make-gemini-request protocol
                                host
                                port
                                path/selector)]
          [(eq? protocol 'http)
           (make-http-request protocol
                              host
                              port
                              path/selector)]
          [else
           (make-gopher-request protocol
                                host
                                port
                                path/selector)]))
      #f))

(define (request->url req)
  (define (protocol->string p)
    (cond
      [(eq? p 'gopher) "gopher://"]
      [(eq? p 'gemini) "gemini://"]
      [(eq? p 'http)   "http://"]
      [else "gopher://"]))

  (define (port-number->string port protocol)
    (cond
      [(eq? protocol 'gopher)
       (if (= port 70)
           ""
           (string-append ":" (number->string port)))]
      [(eq? protocol 'gemini)
       (if (= port 1965)
           ""
           (string-append ":" (number->string port)))]
      [(eq? protocol 'http)
       (if (= port 80)
           ""
           (string-append ":" (number->string port)))]))
  
  (string-append (protocol->string (request-protocol req))
                 (request-host req)
                 (port-number->string (request-port req) (request-protocol req))
                 ;; follow the convention of displaying the type in the URL. if selector is null add a '/' after the type
                 (if (and (eq? (request-protocol req) 'gopher) (request-type req))
                     (if (and (request-path/selector req) (not (equal? (request-path/selector req) "")))
                         (format "/~a" (request-type req))
                         (format "/~a/" (request-type req)))
                     "")
                 (request-path/selector req)))

;; there are two common ways to encode a URL into a gopher selector:
;; prefix with 'URL:' or use 'GET /' as the selector.
(define (gopher-url-request? req)
  (define selector (request-path/selector req))
  (or (string-prefix? selector "URL:")
      (string-prefix? selector "GET /")))

(define (gopher-url-request->url req)
  (define selector (request-path/selector req))
  
  (cond
    [(string-prefix? selector "URL:")
     (string-trim selector "URL:" #:right? #f)]
    [(string-prefix? selector "GET /")
     (string-append "http://" (request-host req) ":" (number->string (request-port req)))]
    [else
     selector]))


;; Helper functions
(define (string->protocol s [default 'gopher])
  (cond
    [(false? s) default]
    [(equal? s "gopher://") 'gopher]
    [(equal? s "gemini://") 'gemini]
    [(equal? s "http://") 'http]
    [else 'unsupported]))

(define (default-port p)
  (cond
    [(eq? p 'gopher) 70]
    [(eq? p 'gemini) 1965]
    [(eq? p 'http) 80]
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

(define (make-gopher-request protocol host port path/selector)
  (request protocol
           host
           (or (and (string? port) (string->number (substring port 1)))
               (default-port protocol))
           (strip-type-from-path path/selector protocol)
           (get-type-from-path path/selector protocol)))

(define (make-gemini-request protocol host port path-plus-query)
  ;; prevent double slash at begining of path, can happen when url ending in slash is concatenated
  ;; with a path starting with a slash
  (define sanitized-path
    (if (string-prefix? path-plus-query "//")
        (substring path-plus-query 1)
        path-plus-query))
  
  (request protocol
           host
           (or (and (string? port) (string->number (substring port 1)))
               (default-port protocol))
           (if (non-empty-string? sanitized-path) sanitized-path "/")
           #f))

(define (make-http-request protocol host port path-plus-query)
  ;; prevent double slash at begining of path, can happen when url ending in slash is concatenated
  ;; with a path starting with a slash
  (define sanitized-path
    (if (string-prefix? path-plus-query "//")
        (substring path-plus-query 1)
        path-plus-query))

  (request protocol
           host
           (or (and (string? port) (string->number (substring port 1)))
               (default-port protocol))
           (if (non-empty-string? sanitized-path) sanitized-path "/")
           #f))
