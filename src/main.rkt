#lang racket/gui

(require "setup-gui.rkt")

(define (remap-port port)
  (cond
    [(eq? (system-type) 'unix) port]
    [(terminal-port? port) port]
    [else
     (open-output-nowhere)]))

(parameterize ([current-output-port (remap-port (current-output-port))]
               [current-error-port  (remap-port (current-error-port))])
  (setup-gui)
  (printf "handling events~n")
  (yield 'wait))
