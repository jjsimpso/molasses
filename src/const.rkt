#lang racket/base
(provide (all-defined-out))

(define molasses-version "0.8.0")
(define molasses-site "https://github.com/jjsimpso/molasses")

(define about-version-string
  (format "Molasses version ~a~nCopyright (C) 2026 Jonathan Simpson~n~nSource code and updates available at ~a"
          molasses-version
          molasses-site))

