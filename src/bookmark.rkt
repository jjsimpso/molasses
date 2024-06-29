#lang racket/gui

(require "config.rkt")

(provide add-bookmark
         populate-bookmark-menu
         load-bookmarks
         save-bookmarks)

(define bookmark-list '())

;; goto-bookmark is a function which takes a url text string
(define (add-bookmark-to-menu url label menu goto-bookmark)
  (new menu-item%
       [parent menu]
       [label label]
       [callback
        (lambda (item ctrl-event)
          (goto-bookmark url))]))

(define (add-bookmark url label menu goto-bookmark)
  #;(eprintf "adding bookmark ~a,~a~n" label url)
  (set! bookmark-list (append bookmark-list (list (cons label url))))
  ;(display bookmark-list)
  (add-bookmark-to-menu url label menu goto-bookmark)
  (save-bookmarks))

(define (populate-bookmark-menu menu goto-bookmark)
  (for ([bookmark (in-list bookmark-list)])
    (add-bookmark-to-menu (cdr bookmark) (car bookmark) menu goto-bookmark)))

(define (load-bookmarks)
  (set! bookmark-list
        (get-preference 'bookmarks
                        (lambda () '())
                        'timestamp
                        bookmarks-file)))

(define (save-bookmarks)
  (put-preferences '(bookmarks) (list bookmark-list) #f bookmarks-file))
