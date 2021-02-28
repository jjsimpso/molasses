#lang racket/base

(require racket/string)
(require racket/port)
(require racket/tcp)
(require net/url-string)

(provide fetch)

(define (url-path->selector pp-list)
  (if (null? pp-list)
      "."
      (string-join
       (map (lambda (pp)
              (path/param-path pp))
            pp-list)
       "/")))

(define (fetch url-string)
  (define url-struct (string->url url-string))
  (define scheme (url-scheme url-struct))
  (define host (url-host url-struct))
  (define path (url-path url-struct))
  (define port (url-port url-struct))

  (eprintf "fetching: ~a, ~a, ~a, ~a~n" scheme host path port)
  (cond
    [(equal? scheme "gopher")
     (define selector (url-path->selector path))
     (gopher-fetch host
                   selector
                   (or port 70))]
    [else #f]))
     


(define (gopher-fetch host selector port)
  (define (send-selector sel-string out)
    (write-bytes (bytes-append
                  (string->bytes/latin-1 sel-string)
                  #"\r\n")
                 out)
    (flush-output out))
  
  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (eprintf "error: ~a" (exn-message exn)))])
    (define-values (in out) (tcp-connect host port))
    (send-selector selector out)
    (define data (port->bytes in))
    (close-output-port out)
    (close-input-port in)
    ;; return data read from port
    data))
