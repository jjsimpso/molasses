#lang racket/gui

(require racket/path)

(provide download-panel%
         track-download
         mark-download-complete)

(struct download
  (thread-id
   data-port
   path
   [state #:mutable])  ; state can be 'active 'cancelled 'completed
)

(define download-list '())

#;(define download-list (list (download 0 (string->path "file1") 'active)
                            (download 0 (string->path "file2") 'active)))

#;(define download-menu-item%
  (class* horizontal-panel% (menu-item<%>)
    (define deleted? #f)
    
    (define/public (delete)
      (set! deleted? #t))
    (define/public (is-deleted?)
      deleted?)
    (define/public (restore)
      (set! deleted? #f))
    
    (super-new)))

#;(define download-menu-item%
  (class menu-item% 
    (init parent tracked-download callback)
    
    (define path (download-path tracked-download))
    (define status (download-state tracked-download))

    (super-new
     [parent parent]
     [label (download->label tracked-download)]
     [callback callback])
    
    (define/override (get-label)
      (super get-label))
    ))

(define download-menu-item%
  (class menu-item% 
    (init-field tracked-dl)
    (super-new)
    (define/public (get-download)
      tracked-dl)))

(define download-panel%
  (class horizontal-panel%
    (inherit popup-menu)
    
    (define update-timer
      (new timer%
           [notify-callback
            (lambda ()
              (eprintf "update-timer timeout~n")
              (when active-menu
                (let ([items (send active-menu get-items)])
                  (for ([item (in-list items)])
                    (send item set-label (download->label (send item get-download))))))
              )]
           [interval #f]
           [just-once? #f]))

    (define active-menu #f)
    
    (define/override (on-subwindow-event receiver evt)
      (cond
        [(send evt button-down?)
         (eprintf "dl = ~a~n" download-list)
         (let ([menu (new popup-menu%
                          [title "Download List"]
                          [popdown-callback
                           ;; stop the timer when the popup menu is dismissed
                           (lambda (pm ce)
                             (set! active-menu #f)
                             (send update-timer stop))])])
           (for ([dl (in-list download-list)])
             (new download-menu-item%
                  [parent menu]
                  [tracked-dl dl]
                  [label (download->label dl)]
                  [callback download-callback]))
           ;; update the transfer status every second
           (send update-timer start 1000)
           (popup-menu menu 0 0)
           (set! active-menu menu))
         #t]
        [else
         (super on-subwindow-event receiver evt)]))
    
    (super-new)))

(define (track-download thread-id data-port path)
  (set! download-list (cons (download thread-id data-port path 'active)
                            download-list)))

(define (mark-download-complete thread-id)
  (define dl (find-download thread-id))
  (set-download-state! dl 'completed))

(define (find-download thread-id)
  (for/first ([dl (in-list download-list)]
              #:when (equal? thread-id
                             (download-thread-id dl)))
    dl))

(define (remove-download thread-id)
  (set! download-list
        (remove thread-id download-list
          (lambda (x y)
            (equal? x (download-thread-id y))))))

(define (download->label dl)
  (define filename (path->string (file-name-from-path (download-path dl))))
  (case (download-state dl)
    [(active)
     (format "Downloading ~a (~a KB), select to cancel" filename
             (quotient (file-position* (download-data-port dl)) 1024))]
    [(cancelled)
     (format "Cancelled ~a, select to remove" filename)]
    [(completed)
     (format "Finished ~a, select to remove" filename)]))

(define (download-callback item ctrl-event)
  (define dl (send item get-download))
  (case (download-state dl)
    [(active) ; cancel the download. kill the thread, delete the file, and remove from the list
     (eprintf "killing thread~a~n" (download-thread-id dl))
     (kill-thread (download-thread-id dl))
     (delete-file (download-path dl))
     (remove-download (download-thread-id dl))]
    [(cancelled completed) ; remove download from the list
     (eprintf "removing ~a~n" (download-path dl))
     (remove-download (download-thread-id dl))]))
