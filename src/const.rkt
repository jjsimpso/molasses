#lang racket/base
(provide (all-defined-out))

(define molasses-version "0.7.4")
(define molasses-site "https://github.com/jjsimpso/molasses")

(define about-version-string
  (format "Molasses version ~a~nCopyright (C) 2024 Jonathan Simpson~n~nSource code and updates available at ~a"
          molasses-version
          molasses-site))

