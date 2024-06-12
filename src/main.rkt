#lang racket/gui

(require "setup-gui.rkt")

(define (remap-port port)
  (cond
    [(eq? (system-type) 'unix) port]
    [(eq? (system-type) 'windows) (open-output-nowhere)]
    [(terminal-port? port) port]
    [else
     (open-output-nowhere)]))

(parameterize ([current-output-port (remap-port (current-output-port))]
               [current-error-port  (remap-port (current-error-port))])
  (setup-gui)
  (printf "handling events~n")
  (yield 'wait)
  (void))


; don't run this file for testing:
(module test racket/base)
