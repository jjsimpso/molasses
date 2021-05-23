#lang racket/gui

(require racket/path)

(provide download-panel%
         track-download
         mark-download-complete)

(struct download
  (thread-id
   path
   [state #:mutable])  ; state can be 'active 'cancelled 'completed
)

(define download-list '())

(define download-panel%
  (class horizontal-panel%
    (inherit popup-menu)
    (define/override (on-subwindow-event receiver evt)
      (cond
        [(send evt button-down?)
         (eprintf "dl = ~a~n" download-list)
         (let ([menu (new popup-menu%
                          [title "Download List"])])
           (for ([dl (in-list download-list)])
             (new menu-item%
                  [parent menu]
                  [label (download->label dl)]
                  [callback (lambda (x y) (eprintf "cancelling ~a~n" (download-path dl)))]))
           #;(new menu-item%
                [parent menu]
                [label "Download 1"]
                [callback (lambda (x y) (eprintf "pressed 1~n"))])
           #;(new menu-item%
                [parent menu]
                [label "Download 2"]
                [callback (lambda (x y) (eprintf "pressed 2~n"))])
           (popup-menu menu 0 0))
         #t]
        [else
         (super on-subwindow-event receiver evt)]))
    (super-new)))

(define (track-download thread-id path)
  (set! download-list (cons (download thread-id path 'active)
                            download-list)))

(define (mark-download-complete thread-id)
  (define dl (find-download thread-id))
  (set-download-state! dl 'completed))

(define (find-download thread-id)
  (for/first ([dl (in-list download-list)]
              #:when (equal? thread-id
                             (download-thread-id dl)))
    dl))

(define (download->label dl)
  (define filename (path->string (file-name-from-path (download-path dl))))
  (case (download-state dl)
    [(active)
     (format "Downloading ~a, select to cancel" filename)]
    [(cancelled)
     (format "Cancelled ~a, select to remove" filename)]
    [(completed)
     (format "Finished ~a, select to remove" filename)]))
