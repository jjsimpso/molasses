#lang racket/gui

(provide (all-defined-out))

(define pref-file (build-path (find-system-path 'pref-dir) "molasses" "config.rktd"))
(define tabs-file (build-path (find-system-path 'pref-dir) "molasses" "saved-tabs.rktd"))
(define bookmarks-file (build-path (find-system-path 'pref-dir) "molasses" "bookmarks.rktd"))

(define home-page-url "gopher://gopher.endangeredsoft.org/1/gopher/home")

(define canvas-smooth-scrolling #t)

(define canvas-light-mode #f)

(define canvas-bg-color (make-color 33 33 33))
(define text-fg-color (send the-color-database find-color "white smoke"))
(define text-bg-color canvas-bg-color)
(define link-color (send the-color-database find-color "lawn green"))
(define link-highlight-color (send the-color-database find-color "yellow"))

(define light-canvas-bg-color (make-color #xFD #xF6 #xE3))
(define light-text-fg-color (make-color #x65 #x7B #x83))
(define light-text-bg-color light-canvas-bg-color)
(define light-link-color (send the-color-database find-color "dodger blue"))
(define light-link-highlight-color (send the-color-database find-color "medium orchid"))

(define html-text-fg-color (send the-color-database find-color "black"))
(define html-text-bg-color (make-color #xFF #xFF #xFF))
(define html-link-color (send the-color-database find-color "blue"))
(define html-vlink-color (send the-color-database find-color "yellow"))


(define (set-canvas-smooth-scrolling! val)
  (set! canvas-smooth-scrolling val))

(define (set-canvas-light-mode! val)
  (set! canvas-light-mode val))
