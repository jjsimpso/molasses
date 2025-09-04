#lang info

(define version "0.7.4")
(define deps (list "base" "gui-lib" "magic" "sxml" "mcfly"
                   "data-lib"
                   "draw-lib"
                   "html-lib"
                   "net-lib"
                   "snip-lib"))
(define build-deps '("rackunit-lib"
                     "overeasy"))
(define license 'LGPL-3.0-or-later)

(define gracket-launcher-names     '("molasses"))
(define gracket-launcher-libraries '("src/main.rkt"))
