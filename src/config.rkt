#lang racket/gui

(provide (all-defined-out))

(define pref-file (build-path (find-system-path 'pref-dir) "molasses" "config.rktd"))
(define tabs-file (build-path (find-system-path 'pref-dir) "molasses" "saved-tabs.rktd"))
(define bookmarks-file (build-path (find-system-path 'pref-dir) "molasses" "bookmarks.rktd"))

(define home-page-url "gopher://gopher.endangeredsoft.org/1/gopher/home")

(define canvas-bg-color (make-color 33 33 33))
(define text-fg-color (send the-color-database find-color "white smoke"))
(define text-bg-color canvas-bg-color)
(define link-color (send the-color-database find-color "lawn green"))
(define link-highlight-color (send the-color-database find-color "yellow"))

(define html-text-fg-color (send the-color-database find-color "black"))
(define html-text-bg-color (make-color #xC0 #xC0 #xC0))
(define html-link-color (send the-color-database find-color "blue"))
(define html-link-highlight-color (send the-color-database find-color "yellow"))
