#lang racket/gui

(provide (all-defined-out))

(define canvas-bg-color (make-color 33 33 33))
(define text-fg-color (send the-color-database find-color "white smoke"))
(define text-bg-color canvas-bg-color)
(define link-color (send the-color-database find-color "lawn green"))
(define link-highlight-color (send the-color-database find-color "yellow"))
