#lang racket/base
;; Copyright Neil Van Dyke.  For legal info, see file "info.rkt".

(require mcfly)

(module+ test
  (require overeasy))

(doc (section "Introduction")

     (para "The "
           (code "html-parsing")
           " library provides a permissive HTML parser.  The parser is useful
for software agent extraction of information from Web pages, for
programmatically transforming HTML files, and for implementing interactive Web
browsers.  "
           (code "html-parsing")
           " emits "
           ;; TODO: 2016-02-21 Once create sxml-doc package, reference that.
           (seclink "top"
                    #:doc '(lib "sxml-intro/sxml-intro.scrbl")
                    #:indirect? #true
                    "SXML/xexp")
           ", so that conventional HTML may be processed with XML tools such as
SXPath.  Like Oleg Kiselyov's "
           (hyperlink "http://pobox.com/~oleg/ftp/Scheme/xml.html#HTML-parser"
                      "SSAX-based HTML parser")
           ", "
           (code "html-parsing")
           " provides a permissive tokenizer, but "
           (code "html-parsing")
           " extends this by attempting to recover syntactic structure.")

     (para "The "
           (code "html-parsing")
           " parsing behavior is permissive in that it accepts erroneous HTML,
handling several classes of HTML syntax errors gracefully, without yielding a
parse error.  This is crucial for parsing arbitrary real-world Web pages, since
many pages actually contain syntax errors that would defeat a strict or
validating parser.  "
           (code "html-parsing")
           "'s handling of errors is intended to generally emulate popular Web
browsers' interpretation of the structure of erroneous HTML.")
     (para (code "html-parsing")
           " also has some support for XHTML, although XML namespace qualifiers
are accepted but stripped from the resulting SXML/xexp.  Note that "
           (italic "valid")
           " XHTML input might be better handled by a validating XML parser
like Kiselyov's SSAX."))

;; BEGIN COPIED FROM XEXP PACKAGE

(define (%html-parsing:make-xexp-char-ref val)
  (if (or (symbol? val) (integer? val))
      `(& ,val)
      (error 'make-xexp-char-ref
             "invalid xexp reference value: ~S"
             val)))

(define %html-parsing:always-empty-html-elements
  '(area base br frame hr img input isindex keygen link meta param
         spacer wbr))

;; END COPIED FROM XEXP PACKAGE

(define %html-parsing:empty-token-symbol       '*empty*)
(define %html-parsing:end-token-symbol         '*end*)
(define %html-parsing:start-token-symbol       '*start*)
(define %html-parsing:entity-token-symbol      '*entity*)
(define %html-parsing:text-string-token-symbol '*text-string*)
(define %html-parsing:text-char-token-symbol   '*text-char*)

(define %html-parsing:make-html-tokenizer
  ;; TODO: Have the tokenizer replace contiguous whitespace within individual
  ;; text tokens with single space characters (except for when in `pre' and
  ;; verbatim elements).  The parser will introduce new contiguous whitespace
  ;; (e.g., when text tokens are concatenated, invalid end tags are removed,
  ;; whitespace is irrelevant between certain elements), but then the parser
  ;; only has to worry about the first and last character of each string.
  ;; Perhaps the text tokens should have both leading and trailing whitespace
  ;; stripped, and contain flags for whether or not leading and trailing
  ;; whitespace occurred.
  (letrec ((no-token '())

           ;; TODO: Maybe make these three variables options.

           (verbatim-to-eof-elems '(plaintext))

           (verbatim-pair-elems '(script server style xmp))

           (ws-chars (list #\space
                           (integer->char 9)
                           (integer->char 10)
                           (integer->char 11)
                           (integer->char 12)
                           (integer->char 13)))

           (gosc/string-or-false
            (lambda (os)
              (let ((s (get-output-string os)))
                (if (string=? s "") #f s))))

           (gosc/symbol-or-false
            (lambda (os)
              (let ((s (gosc/string-or-false os)))
                (if s (string->symbol s) #f))))
           )
    (lambda (in normalized?)
      ;; TODO: Make a tokenizer option that causes XML namespace qualifiers to
      ;; be ignored.
      (letrec
          (
           ;; Port buffer with inexpensive unread of one character and slightly
           ;; more expensive pushback of second character to unread.  The
           ;; procedures themselves do no consing.  The tokenizer currently
           ;; needs two-symbol lookahead, due to ambiguous "/" while parsing
           ;; element and attribute names, which could be either empty-tag
           ;; syntax or XML qualified names.
           (c           #f)
           (next-c      #f)
           (c-consumed? #t)
           (read-c      (lambda ()
                          (if c-consumed?
                              (if next-c
                                  (begin (set! c      next-c)
                                         (set! next-c #f))
                                  (set! c (read-char in)))
                              (set! c-consumed? #t))))
           (unread-c    (lambda ()
                          (if c-consumed?
                              (set! c-consumed? #f)
                              ;; TODO: Procedure name in error message really
                              ;; isn't "%html-parsing:make-html-tokenizer"...
                              (error '%html-parsing:make-html-tokenizer
                                     "already unread: ~S"
                                     c))))
           (push-c      (lambda (new-c)
                          (if c-consumed?
                              (begin (set! c           new-c)
                                     (set! c-consumed? #f))
                              (if next-c
                                  (error '%html-parsing:make-html-tokenizer
                                         "pushback full: ~S"
                                         c)
                                  (begin (set! next-c      c)
                                         (set! c           new-c)
                                         (set! c-consumed? #f))))))

           ;; TODO: These procedures are a temporary convenience for
           ;; enumerating the pertinent character classes, with an eye towards
           ;; removing redundant tests of character class.  These procedures
           ;; should be eliminated in a future version.
           (c-eof?      (lambda () (eof-object? c)))
           (c-amp?      (lambda () (eqv? c #\&)))
           (c-apos?     (lambda () (eqv? c #\')))
           (c-bang?     (lambda () (eqv? c #\!)))
           (c-colon?    (lambda () (eqv? c #\:)))
           (c-quot?     (lambda () (eqv? c #\")))
           (c-equals?   (lambda () (eqv? c #\=)))
           (c-gt?       (lambda () (eqv? c #\>)))
           (c-lsquare?  (lambda () (eqv? c #\[)))
           (c-lt?       (lambda () (eqv? c #\<)))
           (c-minus?    (lambda () (eqv? c #\-)))
           (c-pound?    (lambda () (eqv? c #\#)))
           (c-ques?     (lambda () (eqv? c #\?)))
           (c-semi?     (lambda () (eqv? c #\;)))
           (c-slash?    (lambda () (eqv? c #\/)))
           (c-splat?    (lambda () (eqv? c #\*)))
           (c-lf?       (lambda () (eqv? c #\newline)))
           (c-angle?    (lambda () (memv c '(#\< #\>))))
           (c-ws?       (lambda () (memv c ws-chars)))
           (c-alpha?    (lambda () (char-alphabetic? c)))
           (c-digit?    (lambda () (char-numeric? c)))
           (c-alphanum? (lambda () (or (c-alpha?) (c-digit?))))
           (c-hexlet?   (lambda () (memv c '(#\a #\b #\c #\d #\e #\f
                                             #\A #\B #\C #\D #\E #\F))))

           (skip-ws     (lambda () (read-c) (if (c-ws?) (skip-ws) (unread-c))))

           (if-read-chars
            (lambda (match-chars yes-thunk no-proc)
              (let loop ((chars       match-chars)
                         (match-count 0))
                (if (null? chars)
                    (yes-thunk)
                    (begin (read-c)
                           (if (eqv? c (car chars))
                               (begin (loop (cdr chars) (+ 1 match-count)))
                               (begin (unread-c)
                                      (no-proc match-chars match-count))))))))

           (write-chars-count
            (lambda (chars count port)
              (let loop ((chars chars)
                         (count count))
                (or (zero? count)
                    (begin (write-char (car chars) port)
                           (loop (cdr chars)
                                 (- count 1)))))))

           (make-start-token
            (if normalized?
                (lambda (name ns attrs)
                  (list name (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list name)
                      (list name (cons '@ attrs))))))

           (make-empty-token
            (lambda (name ns attrs)
              (cons %html-parsing:empty-token-symbol
                    (make-start-token name ns attrs))))

           (make-end-token
            (if normalized?
                (lambda (name ns attrs)
                  (list %html-parsing:end-token-symbol
                        name
                        (cons '@ attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list %html-parsing:end-token-symbol name)
                      (list %html-parsing:end-token-symbol
                            name
                            (cons '@ attrs))))))

           (make-comment-token
            (lambda (str) (list '*COMMENT* str)))

           (make-decl-token
            (lambda (parts) (cons '*DECL* parts)))

           (scan-qname
            ;; TODO: Make sure we don't accept local names that have "*", since
            ;; this can break SXML tools.  Have to validate this afterwards if
            ;; "verbatim-safe?".  Also check for "@" and maybe "@@".  Check
            ;; qname parsing code, especially for verbatim mode.  This is
            ;; important!
            (lambda (verbatim-safe?)
              ;; Note: If we accept some invalid local names, we only need two
              ;; symbols of lookahead to determine the end of a qname.
              (letrec ((os      #f)
                       (ns      '())
                       (vcolons 0)
                       (good-os (lambda ()
                                  (or os
                                      (begin (set! os (open-output-string))
                                             os)))))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((or (c-ws?) (c-splat?))
                         (if verbatim-safe?
                             (unread-c)
                             #f))
                        ((or (c-angle?) (c-equals?) (c-quot?) (c-apos?))
                         (unread-c))
                        ((c-colon?)
                         (or (null? ns)
                             (set! ns (cons ":" ns)))
                         (if os
                             (begin
                               (set! ns (cons (get-output-string os)
                                              ns))
                               (set! os #f))
                             #f)
                         (loop))
                        ((c-slash?)
                         (read-c)
                         (cond ((or (c-eof?)
                                    (c-ws?)
                                    (c-equals?)
                                    (c-apos?)
                                    (c-quot?)
                                    (c-angle?)
                                    (c-splat?))
                                (unread-c)
                                (push-c #\/))
                               (else (write-char #\/ (good-os))
                                     (write-char c   os)
                                     (loop))))
                        (else (write-char c (good-os))
                              (loop))))
                (let ((ns    (if (null? ns)
                                 #f
                                 (apply string-append
                                        (reverse ns))))
                      (localname (if os (get-output-string os) #f)))
                  (if verbatim-safe?
                      ;; TODO: Make sure we don't have ambiguous ":" or drop
                      ;; any characters!
                      (cons ns localname)
                      ;; Note: We represent "xml:" and "xmlns:" syntax as
                      ;; normal qnames, for lack of something better to do with
                      ;; them when we don't support XML namespaces.
                      ;;
                      ;; TODO: Local names are currently forced to lowercase,
                      ;; since HTML is usually case-insensitive.  If XML
                      ;; namespaces are used, we might wish to keep local names
                      ;; case-sensitive.
                      (if localname
                          (if ns
                              (if (or (string=? ns "xml")
                                      (string=? ns "xmlns"))
                                  (string->symbol (string-append ns
                                                                 ":"
                                                                 localname))
                                  (cons ns
                                        (string->symbol (string-downcase
                                                         localname))))
                              (string->symbol (string-downcase localname)))
                          (if ns
                              (string->symbol (string-downcase ns))
                              ;; TODO: Ensure in rest of code that returning #f
                              ;; as a name here is OK.
                              #f)))))))

           (scan-tag
            (lambda (start?)
              (skip-ws)
              (let ((tag-name   (scan-qname #f))
                    (tag-ns     #f)
                    (tag-attrs  #f)
                    (tag-empty? #f))
                ;; Scan element name.
                (if (pair? tag-name)
                    (begin (set! tag-ns   (car tag-name))
                           (set! tag-name (cdr tag-name)))
                    #f)
                ;; TODO: Ensure there's no case in which a #f tag-name isn't
                ;; compensated for later.
                ;;
                ;; Scan element attributes.
                (set! tag-attrs
                      (let scan-attr-list ()
                        (read-c)
                        (cond ((c-eof?)   '())
                              ((c-angle?) (unread-c) '())
                              ((c-slash?)
                               (set! tag-empty? #t)
                               (scan-attr-list))
                              ((c-alpha?)
                               (unread-c)
                               (let ((attr (scan-attr)))
                                 (cons attr (scan-attr-list))))
                              (else (scan-attr-list)))))
                ;; Find ">" or unnatural end.
                (let loop ()
                  (read-c)
                  (cond ((c-eof?)   no-token)
                        ((c-slash?) (set! tag-empty? #t) (loop))
                        ((c-gt?)    #f)
                        ((c-ws?)    (loop))
                        (else       (unread-c))))
                ;; Change the tokenizer mode if necessary.
                (cond ((not start?) #f)
                      (tag-empty?   #f)
                      ;; TODO: Maybe make one alist lookup here, instead of
                      ;; two.
                      ((memq tag-name verbatim-to-eof-elems)
                       (set! nexttok verbeof-nexttok))
                      ((memq tag-name verbatim-pair-elems)
                       (set! nexttok (make-verbpair-nexttok tag-name))))
                ;; Return a token object.
                (if start?
                    (if tag-empty?
                        (make-empty-token tag-name tag-ns tag-attrs)
                        (make-start-token tag-name tag-ns tag-attrs))
                    (make-end-token tag-name tag-ns tag-attrs)))))

           (scan-attr
            (lambda ()
              (let ((name (scan-qname #f))
                    (val  #f))
                (if (pair? name)
                    (set! name (cdr name))
                    #f)
                (let loop-equals-or-end ()
                  (read-c)
                  (cond ((c-eof?) no-token)
                        ((c-ws?)  (loop-equals-or-end))
                        ((c-equals?)
                         (let loop-quote-or-unquoted ()
                           (read-c)
                           (cond ((c-eof?) no-token)
                                 ((c-ws?) (loop-quote-or-unquoted))
                                 ((or (c-apos?) (c-quot?))
                                  (let ((term c))
                                    (set! val (open-output-string))
                                    (let loop-quoted-val ()
                                      (read-c)
                                      (cond ((c-eof?)      #f)
                                            ((eqv? c term) #f)
                                            (else (write-char c val)
                                                  (loop-quoted-val))))))
                                 ((c-angle?) (unread-c))
                                 (else
                                  (set! val (open-output-string))
                                  (write-char c val)
                                  (let loop-unquoted-val ()
                                    (read-c)
                                    (cond ((c-eof?)  no-token)
                                          ((c-apos?) #f)
                                          ((c-quot?) #f)
                                          ((or (c-ws?) (c-angle?)
                                               ;;(c-slash?)
                                               )
                                           (unread-c))
                                          ;; Note: We can treat a slash in an
                                          ;; unquoted attribute value as a
                                          ;; value constituent because the
                                          ;; slash is specially-handled only
                                          ;; for XHTML, and XHTML attribute
                                          ;; values must always be quoted.  We
                                          ;; could do lookahead for "/>", but
                                          ;; that wouldn't let us parse HTML
                                          ;; "<a href=/>" correctly, so this is
                                          ;; an easier and more correct way to
                                          ;; do things.
                                          (else (write-char c val)
                                                (loop-unquoted-val))))))))
                        (else (unread-c))))
                (if normalized?
                    (list name (if val
                                   (get-output-string val)
                                   (symbol->string name)))
                    (if val
                        (list name (get-output-string val))
                        (list name))))))

           (scan-comment
            ;; TODO: Rewrite this to use tail recursion rather than a state
            ;; variable.
            (lambda ()
              (let ((os    (open-output-string))
                    (state 'start-minus))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((c-minus?)
                         (set! state
                               (case state
                                 ((start-minus)            'start-minus-minus)
                                 ((start-minus-minus body) 'end-minus)
                                 ((end-minus)              'end-minus-minus)
                                 ((end-minus-minus) (write-char #\- os) state)
                                 (else (error '<%html-parsing:make-html-tokenizer>
                                              "invalid state: ~S"
                                              state))))
                         (loop))
                        ((and (c-gt?) (eq? state 'end-minus-minus)) #f)
                        (else (case state
                                ((end-minus)       (write-char #\- os))
                                ((end-minus-minus) (display "--" os)))
                              (set! state 'body)
                              (write-char c os)
                              (loop))))
                (make-comment-token (get-output-string os)))))

           (scan-possible-cdata
            (lambda ()
              ;; Read "<!" and current character is "[", so try to read the
              ;; rest of the CDATA start delimeter.
              (if-read-chars
               '(#\C #\D #\A #\T #\A #\[)
               (lambda ()
                 ;; Successfully read CDATA section start delimiter, so read
                 ;; the section.
                 (scan-cdata))
               (lambda (chars count)
                 ;; Did not read rest of CDATA section start delimiter, so
                 ;; return a string for what we did read.
                 (let ((os (open-output-string)))
                   (display "<![" os)
                   (write-chars-count chars count os)
                   (get-output-string os))))))

           (scan-cdata
            (lambda ()
              (let ((os (open-output-string)))
                (let loop ()
                  (if-read-chars
                   '(#\] #\] #\>)
                   (lambda () (get-output-string os))
                   (lambda (chars count)
                     (if (zero? count)
                         (if (eof-object? c)
                             (get-output-string os)
                             (begin (write-char c os)
                                    (read-c)
                                    (loop)))
                         (begin (write-char #\] os)
                                (if (= count 2)
                                    (push-c #\])
                                    #f)
                                (loop)))))))))

           (scan-pi
            (lambda ()
              (skip-ws)
              (let ((name (open-output-string))
                    (val  (open-output-string)))
                (let scan-name ()
                  (read-c)
                  (cond ((c-eof?)   #f)
                        ((c-ws?)    #f)
                        ((c-alpha?) (write-char c name) (scan-name))
                        (else       (unread-c))))
                ;; TODO: Do we really want to emit #f for PI name?
                (set! name (gosc/symbol-or-false name))
                (let scan-val ()
                  (read-c)
                  (cond ((c-eof?)  #f)
                        ;; ((c-amp?) (display (scan-entity) val)
                        ;;           (scan-val))
                        ((c-ques?)
                         (read-c)
                         (cond ((c-eof?) (write-char #\? val))
                               ((c-gt?)  #f)
                               (else     (write-char #\? val)
                                         (unread-c)
                                         (scan-val))))
                        (else (write-char c val) (scan-val))))
                (list '*PI*
                      name
                      (get-output-string val)))))

           (scan-decl
            ;; TODO: Find if SXML includes declaration forms, and if so, use
            ;; whatever format SXML wants.
            ;;
            ;; TODO: Rewrite to eliminate state variables.
            (letrec
                ((scan-parts
                  (lambda ()
                    (let ((part       (open-output-string))
                          (nonsymbol? #f)
                          (state      'before)
                          (last?      #f))
                      (let loop ()
                        (read-c)
                        (cond ((c-eof?) #f)
                              ((c-ws?)
                               (case state
                                 ((before) (loop))
                                 ((quoted) (write-char c part) (loop))))
                              ((and (c-gt?) (not (eq? state 'quoted)))
                               (set! last? #t))
                              ((and (c-lt?) (not (eq? state 'quoted)))
                               (unread-c))
                              ((c-quot?)
                               (case state
                                 ((before)   (set! state 'quoted) (loop))
                                 ((unquoted) (unread-c))
                                 ((quoted)   #f)))
                              (else
                               (if (eq? state 'before)
                                   (set! state 'unquoted)
                                   #f)
                               (set! nonsymbol? (or nonsymbol?
                                                    (not (c-alphanum?))))
                               (write-char c part)
                               (loop))))
                      (set! part (get-output-string part))
                      (if (string=? part "")
                          '()
                          (cons (if (or (eq? state 'quoted) nonsymbol?)
                                    part
                                    ;; TODO: Normalize case of things we make
                                    ;; into symbols here.
                                    (string->symbol part))
                                (if last?
                                    '()
                                    (scan-parts))))))))
              (lambda () (make-decl-token (scan-parts)))))

           (special-entity-reverse-chars-to-string-alist
            '(((#\p #\m #\a)     . "&")
              ((#\s #\o #\p #\a) . "'")
              ((#\t #\g)         . ">")
              ((#\t #\l)         . "<")
              ((#\t #\o #\u #\q) . "\"")))
           
           (finish-terminated-named-entity
            (lambda (reverse-name-chars)
              (cond ((equal? '() reverse-name-chars)
                     "&")
                    ((assoc reverse-name-chars
                            special-entity-reverse-chars-to-string-alist)
                     => (lambda (p)
                          (cdr p)))
                    (else (%html-parsing:make-xexp-char-ref 
                           (string->symbol (apply string (reverse reverse-name-chars))))))))
           
           (finish-unterminated-named-entity
            (lambda (reverse-name-chars)
              (apply string (cons #\& (reverse reverse-name-chars)))))
           
           (scan-entity
            (lambda ()
              (read-c)
              (cond ((c-eof?) "&")
                    ((c-alpha?)
                     ;; TODO: Do entity names have a maximum length?
                       (let loop ((reverse-name-chars (cons c '())))
                         (read-c)
                         (cond ((c-eof?)   (finish-unterminated-named-entity
                                            reverse-name-chars))
                               ((c-alpha?) (let ((reverse-name-chars (cons c reverse-name-chars)))
                                             (cond ((assoc reverse-name-chars
                                                           special-entity-reverse-chars-to-string-alist)
                                                    => (lambda (p)
                                                         (read-c)
                                                         (or (c-semi?)
                                                             (unread-c))
                                                         (cdr p)))
                                                   (else (loop reverse-name-chars)))))
                               ((c-semi?)  (finish-terminated-named-entity
                                            reverse-name-chars))
                               (else       (unread-c)
                                           (finish-unterminated-named-entity
                                            reverse-name-chars)))))
                    ((c-pound?)
                     (let ((num  (open-output-string))
                           (hex? #f))
                       (read-c)
                       (cond ((c-eof?)            #f)
                             ((memv c '(#\x #\X)) (set! hex? #t) (read-c)))
                       (let loop ()
                         (cond ((c-eof?)  #f)
                               ((c-semi?) #f)
                               ((or (c-digit?) (and hex? (c-hexlet?)))
                                (write-char c num)
                                (read-c)
                                (loop))
                               (else (unread-c))))
                       (set! num (get-output-string num))
                       (if (string=? num "")
                           "&#;"
                           (let ((n (string->number num (if hex? 16 10))))
                             (if (<= 32 n 126)
                                 (string (integer->char n))
                                 (string (integer->char n)))))))
                    (else (unread-c) "&"))))

           (normal-nexttok
            (lambda ()
              (read-c)
              (cond ((c-eof?) no-token)
                    ((c-lt?)
                     (let loop ()
                       (read-c)
                       (cond ((c-eof?)   "<")
                             ;; ((c-ws?)    (loop))
                             ((c-slash?) (scan-tag #f))
                             ((c-ques?)  (scan-pi))
                             ((c-alpha?) (unread-c) (scan-tag #t))
                             ((c-bang?)
                              (read-c)
                              (if (c-lsquare?)
                                  (scan-possible-cdata)
                                  (let loop ()
                                    (cond ((c-eof?)   no-token)
                                          ((c-ws?)    (read-c) (loop))
                                          ((c-minus?) (scan-comment))
                                          (else       (unread-c)
                                                      (scan-decl))))))
                             (else (unread-c) "<"))))
                    ((c-gt?) ">")
                    (else (let ((os (open-output-string)))
                            (let loop ()
                              (cond ((c-eof?)   #f)
                                    ((c-angle?) (unread-c))
                                    ((c-amp?)
                                     (let ((entity (scan-entity)))
                                       (if (string? entity)
                                           (begin (display entity os)
                                                  (read-c)
                                                  (loop))
                                           (let ((saved-nexttok nexttok))
                                             (set! nexttok
                                                   (lambda ()
                                                     (set! nexttok
                                                           saved-nexttok)
                                                     entity))))))
                                    (else (write-char c os)
                                          (or (c-lf?)
                                              (begin (read-c) (loop))))))
                            (let ((text (get-output-string os)))
                              (if (equal? text "")
                                  (nexttok)
                                  text)))))))

           (verbeof-nexttok
            (lambda ()
              (read-c)
              (if (c-eof?)
                  no-token
                  (let ((os (open-output-string)))
                    (let loop ()
                      (or (c-eof?)
                          (begin (write-char c os)
                                 (or (c-lf?)
                                     (begin (read-c) (loop))))))
                    (get-output-string os)))))

           (make-verbpair-nexttok
            (lambda (elem-name)
              (lambda ()
                (let ((os (open-output-string)))
                  ;; Accumulate up to a newline-terminated line.
                  (let loop ()
                    (read-c)
                    (cond ((c-eof?)
                           ;; Got EOF in verbatim context, so set the normal
                           ;; nextok procedure, then fall out of loop.
                           (set! nexttok normal-nexttok))
                          ((c-lt?)
                           ;; Got "<" in verbatim context, so get next
                           ;; character.
                           (read-c)
                           (cond ((c-eof?)
                                  ;; Got "<" then EOF, so set to the normal
                                  ;; nexttok procedure, add the "<" to the
                                  ;; verbatim string, and fall out of loop.
                                  (set! nexttok normal-nexttok)
                                  (write-char #\< os))
                                 ((c-slash?)
                                  ;; Got "</", so...
                                  (read-c)
                                  (cond
                                   ((c-eof?)
                                    (display "</" os))
                                   ((c-alpha?)
                                    ;; Got "</" followed by alpha, so unread
                                    ;; the alpha, scan qname, compare...
                                    (unread-c)
                                    (let* ((vqname (scan-qname #t))
                                           (ns     (car vqname))
                                           (local  (cdr vqname)))
                                      ;; Note: We ignore XML namespace
                                      ;; qualifier for purposes of comparison.
                                      ;;
                                      ;; Note: We're interning strings here for
                                      ;; comparison when in theory there could
                                      ;; be many such unique interned strings
                                      ;; in a valid HTML document, although in
                                      ;; practice this should not be a problem.
                                      (if (and local
                                               (eqv? (string->symbol
                                                      (string-downcase local))
                                                     elem-name))
                                          ;; This is the terminator tag, so
                                          ;; scan to the end of it, set the
                                          ;; nexttok, and fall out of the loop.
                                          (begin
                                            (let scan-to-end ()
                                              (read-c)
                                              (cond ((c-eof?) #f)
                                                    ((c-gt?)  #f)
                                                    ((c-lt?)  (unread-c))
                                                    ((c-alpha?)
                                                     (unread-c)
                                                     ;; Note: This is an
                                                     ;; expensive way to skip
                                                     ;; over an attribute, but
                                                     ;; in practice more
                                                     ;; verbatim end tags will
                                                     ;; not have attributes.
                                                     (scan-attr)
                                                     (scan-to-end))
                                                    (else (scan-to-end))))
                                            (set! nexttok
                                                  (lambda ()
                                                    (set! nexttok
                                                          normal-nexttok)
                                                    (make-end-token
                                                     elem-name #f '()))))
                                          ;; This isn't the terminator tag, so
                                          ;; add to the verbatim string the
                                          ;; "</" and the characters of what we
                                          ;; were scanning as a qname, and
                                          ;; recurse in the loop.
                                          (begin
                                            (display "</" os)
                                            (if ns
                                                (begin (display ns os)
                                                       (display ":" os))
                                                #f)
                                            (if local
                                                (display local os)
                                                #f)
                                            (loop)))))
                                   (else
                                    ;; Got "</" and non-alpha, so unread new
                                    ;; character, add the "</" to verbatim
                                    ;; string, then loop.
                                    (unread-c)
                                    (display "</" os)
                                    (loop))))
                                 (else
                                  ;; Got "<" and non-slash, so unread the new
                                  ;; character, write the "<" to the verbatim
                                  ;; string, then loop.
                                  (unread-c)
                                  (write-char #\< os)
                                  (loop))))
                          (else
                           ;; Got non-"<" in verbatim context, so just add it
                           ;; to the buffer, then, if it's not a linefeed, fall
                           ;; out of the loop so that the token can be
                           ;; returned.
                           (write-char c os)
                           (or (c-lf?) (loop)))))
                  ;; Return the accumulated line string, if non-null, or call
                  ;; nexttok.
                  (or (gosc/string-or-false os) (nexttok))))))

           (nexttok #f))

        (set! nexttok normal-nexttok)
        (lambda () (nexttok))))))

(define (%html-parsing:tokenize-html in normalized?)
  (let ((next-tok (%html-parsing:make-html-tokenizer in normalized?)))
    (let loop ((tok (next-tok)))
      (if (null? tok)
          '()
          (cons tok (loop (next-tok)))))))

(define (%html-parsing:xexp-token-kind token)
  (cond ((string? token) %html-parsing:text-string-token-symbol)
        ((char?   token) %html-parsing:text-char-token-symbol)
        ((list?   token)
         (let ((s (car token)))
           (if (memq s `(*COMMENT*
                         *DECL*
                         *PI*
                         ,%html-parsing:empty-token-symbol
                         ,%html-parsing:end-token-symbol
                         ,%html-parsing:entity-token-symbol))
               s
               %html-parsing:start-token-symbol)))
        (else (error '%html-parsing:xexp-token-kind
                     "unrecognized token kind: ~S"
                     token))))

(doc (section "Interface"))

;; @defvar %html-parsing:empty-elements
;;
;; List of names of HTML element types that have no content, represented as a
;; list of symbols.  This is used internally by the parser and encoder.  The
;; effect of mutating this list is undefined.

;; TODO: Document exactly which elements these are, after we make the new
;; parameterized parser constructor.

(define %html-parsing:empty-elements
  (cons '& %html-parsing:always-empty-html-elements))

;; @defproc parse-html/tokenizer tokenizer normalized?
;;
;; Emits a parse tree like @code{html->xexp} and related procedures, except
;; using @var{tokenizer} as a source of tokens, rather than tokenizing from an
;; input port.  This procedure is used internally, and generally should not be
;; called directly.

(define %html-parsing:parse-html/tokenizer
  ;; Note: This algorithm was originally written in 2001 (as part of the first
  ;; Scheme library the author ever wrote), and then on 2009-08-16 was revamped
  ;; to not use mutable pairs, for PLT 4 compatibility.  It could still use
  ;; some work to be more FP, but it works for now.
  (letrec ((empty-elements
            ;; TODO: Maybe make this an option.
            %html-parsing:empty-elements)
           (h-elem-parents
            ;; Am doing this kludge mainly for mid-1990s HTML that uses the `p`
            ;; element wrong.  Trying to get all appropriate parents other than
            ;; `p` that I can, to reduce breaking other code.
            '(a article aside blink blockquote body center footer form header html li main nav pre section slot td th template))
           (parent-constraints
            ;; TODO: Maybe make this an option.
            `((area     . (map span))
              (body     . (html))
              (caption  . (table))
              (colgroup . (table))
              (dd       . (dl))
              (dt       . (dl))
              (frame    . (frameset))
              (head     . (html))
              (h1       . ,h-elem-parents)
              (h2       . ,h-elem-parents)
              (h3       . ,h-elem-parents)
              (h4       . ,h-elem-parents)
              (h5       . ,h-elem-parents)
              (h6       . ,h-elem-parents)
              ;; in netscape hr seems to close text level tags like font, etc.
              (hr       . (blockquote body center details div h1 h2 h3 h4 h5 h6 html li p pre td th))
              (isindex  . (head))
              (li       . (dir menu ol ul))
              (meta     . (head))
              (noframes . (frameset))
              (option   . (select))
              (p        . (blockquote body details html li td th center))
              (param    . (applet))
              (tbody    . (table))
              (td       . (tr))
              (th       . (tr))
              (thead    . (table))
              (title    . (head))
              (tr       . (table tbody thead))))
           (token-kinds-that-always-get-added
            `(*COMMENT*
              *DECL*
              *PI*
              ,%html-parsing:entity-token-symbol
              ,%html-parsing:text-string-token-symbol
              ,%html-parsing:text-char-token-symbol))
           (start-tag-name (lambda (tag-token) (car tag-token)))
           (end-tag-name   (lambda (tag-token) (list-ref tag-token 1))))
    (lambda (tokenizer normalized?)
      ;;(log-html-parsing-debug "(%html-parsing:parse-html/tokenizer ~S ~S)" tokenizer normalized?)
      (let ((begs (list (vector #f '()))))
        (letrec ((add-thing-as-child-of-current-beg
                  (lambda (tok)
                    (let ((beg (car begs)))
                      (vector-set! beg 1 (cons tok (vector-ref beg 1))))))

                 (beg->elem
                  (lambda (beg)
                    (let ((elem-name          (vector-ref beg 0))
                          (attrs-and-contents (reverse (vector-ref beg 1))))
                      (cons elem-name attrs-and-contents))))

                 (finish-current-beg-and-return-elem
                  (lambda ()
                    (let ((elem (beg->elem (car begs))))
                      (set! begs (cdr begs))
                      (or (null? begs)
                          (add-thing-as-child-of-current-beg elem))
                      elem)))

                 (finish-current-beg
                  (lambda ()
                    (finish-current-beg-and-return-elem)))

                 (finish-all-begs-and-return-top
                  (lambda ()
                    (let loop ()
                      (let ((elem (finish-current-beg-and-return-elem)))
                        (if (car elem)
                            (loop)
                            (cdr elem))))))

                 (finish-begs-up-to-and-including-name
                  (lambda (name)
                    ;; (log-html-parsing-debug "(finish-begs-up-to-and-including-name ~S)" name)
                    (let loop-find-name ((find-begs begs)
                                         (depth     1))
                      (let ((beg-name (vector-ref (car find-begs) 0)))
                        (cond ((not beg-name)
                               ;; We reached the root without finding a
                               ;; matching beg, so don't finish anything.
                               ;;
                               ;; TODO: 2022-04-02: Consider having a `*TOP*`
                               ;; kludge in `parent-constraints` that's checked
                               ;; here, especially for handling mid-1990s HTML
                               ;; `p` element (so that we can keep `p` from
                               ;; being a child of `p` even when there's no
                               ;; parent `body` or `html` element).
                               (void))
                              ((eqv? name beg-name)
                               ;; We found a match, so finish the begs up to
                               ;; depth.
                               (let loop-finish ((depth depth))
                                 (or (zero? depth)
                                     (begin
                                       (finish-current-beg)
                                       (loop-finish (- depth 1))))))
                              (else
                               ;; Didn't find a match yet, and there's still at
                               ;; least one more beg to look at, so recur.
                               (loop-find-name (cdr find-begs)
                                               (+ depth 1))))))))

                 (finish-begs-upto-but-not-including-names
                  (lambda (names)
                    ;; (log-html-parsing-debug "(finish-begs-upto-but-not-including-names ~S)" names)
                    ;; (log-html-parsing-debug "begs = ~S" begs)
                    (let loop-find-name ((find-begs begs)
                                         (depth     0))
                      (let ((beg-name (vector-ref (car find-begs) 0)))
                        (cond ((not beg-name)
                               ;; We reached the root without finding a
                               ;; matching beg, so simply discard it.
                               (void))
                              ((memq beg-name names)
                               ;; We found a match, so finish the begs up to
                               ;; depth.
                               (let loop-finish ((depth depth))
                                 (or (zero? depth)
                                     (begin
                                       (finish-current-beg)
                                       (loop-finish (- depth 1))))))
                              (else
                               ;; Didn't find a match yet, and there's still at
                               ;; least one more beg to look at, so recur.
                               (loop-find-name (cdr find-begs)
                                               (+ depth 1)))))))))

          (let loop ()
            (let ((tok (tokenizer)))
              (if (null? tok)
                  (finish-all-begs-and-return-top)
                  (let ((kind (%html-parsing:xexp-token-kind tok)))
                    ;; (log-html-parsing-debug "kind = ~S" kind)
                    (cond ((memv kind token-kinds-that-always-get-added)
                           (add-thing-as-child-of-current-beg tok))
                          ((eqv? kind %html-parsing:start-token-symbol)
                           ;; (log-html-parsing-debug "is-start-token-symbol")
                           (let* ((name (start-tag-name tok))
                                  (cell (assq name parent-constraints)))
                             ;; (log-html-parsing-debug "name = ~S cell = ~S" name cell)
                             (and cell
                                  (finish-begs-upto-but-not-including-names
                                   (cons 'div (cdr cell))))
                             (if (memq name empty-elements)
                                 (add-thing-as-child-of-current-beg tok)
                                 (set! begs (cons (vector (car tok)
                                                          (cdr tok))
                                                  begs)))))
                          ((eqv? kind %html-parsing:empty-token-symbol)
                           ;; Empty tag token, so just add it to current
                           ;; beginning while stripping off leading `*EMPTY*'
                           ;; symbol so that the token becomes normal SXML
                           ;; element syntax.
                           (add-thing-as-child-of-current-beg (cdr tok)))
                          ((eqv? kind %html-parsing:end-token-symbol)
                           (let ((name (end-tag-name tok)))
                             (if name
                                 ;; Try to finish to a start tag matching this
                                 ;; end tag.  If none, just drop the token,
                                 ;; though we used to add it to the current
                                 ;; beginning.
                                 (finish-begs-up-to-and-including-name
                                  name)
                                 ;; We have an anonymous end tag, so match it
                                 ;; with the most recent beginning.  If no
                                 ;; beginning to match, then just drop the
                                 ;; token, though we used to add it to the
                                 ;; current beginning.
                                 (and (vector-ref (car begs) 0)
                                      (finish-current-beg)))))
                          (else (error 'parse-html/tokenizer
                                       "unknown tag kind: ~S"
                                       kind)))
                    (loop))))))))))

;; TODO: Quote of message to a user:
;;
;; >I think this behavior is due to HtmlPrag's use in "parse-html/tokenizer"
;; >of its local "parent-constraints" variable.
;; >
;; >The following line of code from the variable binding expresses the
;; >constraint that any "p" element can have as immediate parent element
;; >only "body", "td", or "th":
;; >
;; >              (p        . (body td th))
;; >
;; >I think I know a good heuristic for dealing with unfamiliar but
;; >seemingly well-formed elements, like "page" in this case, but I'm afraid
;; >I don't have time to implement it right now.  (I am job-hunting right
;; >now, and there are many other coding things I need to do first.)
;; >
;; >Would adding "page" to the above line of the HtmlPrag source code work
;; >around the current problem, or do you need a better solution right now?

;; @defproc %parse-html input normalized? top?
;;
;; This procedure is now used internally by @code{html->xexp} and its
;; variants, and should not be used directly by programs.  The interface is
;; likely to change in future versions of HtmlPrag.

(define (%html-parsing:parse-html input normalized? top?)
  (let ((parse
         (lambda ()
           (%html-parsing:parse-html/tokenizer
            (%html-parsing:make-html-tokenizer
             (cond ((input-port? input) input)
                   ((string?     input) (open-input-string input))
                   (else (error
                          '%html-parsing:parse-html
                          "invalid input type: ~E"
                          input)))
             normalized?)
            normalized?))))
    (if top?
        (cons '*TOP* (parse))
        (parse))))

;; @defproc  html->sxml-0nf input
;; @defprocx html->sxml-1nf input
;; @defprocx html->sxml-2nf input
;; @defprocx html->sxml     input
(doc (defproc (html->xexp (input (or/c input-port? string?)))
         xexp

       (para "Parse HTML permissively from "
             (racket input)
             ", which is either an input port or a string, and emit an
SXML/xexp equivalent or approximation.  To borrow and slightly modify an
example from Kiselyov's discussion of his HTML parser:")

       (racketinput
        (html->xexp
         (string-append
          "<html><head><title></title><title>whatever</title></head>"
          "<body> <a href=\"url\">link</a><p align=center>"
          "<ul compact style=\"aa\"> <p>BLah<!-- comment <comment> -->"
          " <i> italic <b> bold <tt> ened</i> still &lt; bold </b>"
          "</body><P> But not done yet...")))
       (racketresultblock
        (*TOP* (html (head (title) (title "whatever"))
                     (body "\n"
                           (a (@ (href "url")) "link")
                           (p (@ (align "center"))
                              (ul (@ (compact) (style "aa")) "\n"))
                           (p "BLah"
                              (*COMMENT* " comment <comment> ")
                              " "
                              (i " italic " (b " bold " (tt " ened")))
                              "\n"
                              "still < bold "))
                     (p " But not done yet..."))))

       (para "Note that, in the emitted SXML/xexp, the text token "
             (racket "still < bold")
             " is "
             (italic "not")
             " inside the "
             (racketfont "b")
             " element.  This is one old Web browser quirk-handling of invalid
HTML that this parser does not try to emulate.")))
;; (: html->xexp ((U String InputPort) -> Xexp))
(provide html->xexp)
(define (html->xexp input)
  (%html-parsing:parse-html input #f #t))

;; (define-logger html-parsing)

(module+ test

  (test (html->xexp "<a>>") '(*TOP* (a ">")))
  (test (html->xexp "<a<>") '(*TOP* (a "<" ">")))

  (test (html->xexp "<>")      '(*TOP* "<" ">"))
  (test (html->xexp "< >")
        ;; `(*TOP* "<" ">")
        '(*TOP* "<" " " ">"))
  (test (html->xexp "< a>")
        ;; `(*TOP* (a))
        '(*TOP*  "<" " a" ">"))
  (test (html->xexp "< a / >")
        ;; `(*TOP* (a))
        '(*TOP* "<" " a / " ">"))

  (test (html->xexp "<a<")  '(*TOP* (a "<")))
  (test (html->xexp "<a<b") '(*TOP* (a (b))))

  (test (html->xexp "><a>") '(*TOP* ">" (a)))

  (test (html->xexp "</>") '(*TOP*))

  (test (html->xexp "<\">") '(*TOP* "<" "\"" ">"))

  (test (html->xexp (string-append "<a>xxx<plaintext>aaa" "\n"
                                   "bbb" "\n"
                                   "c<c<c"))
        `(*TOP*
          (a "xxx" (plaintext ,(string-append "aaa" "\n")
                              ,(string-append "bbb" "\n")
                              "c<c<c"))))

  (test (html->xexp "aaa<!-- xxx -->bbb")
        `(*TOP*
          "aaa" (*COMMENT* " xxx ")   "bbb"))

  (test (html->xexp "aaa<! -- xxx -->bbb")
        `(*TOP*
          "aaa" (*COMMENT* " xxx ")   "bbb"))

  (test (html->xexp "aaa<!-- xxx --->bbb")
        `(*TOP*
          "aaa" (*COMMENT* " xxx -")  "bbb"))

  (test (html->xexp "aaa<!-- xxx ---->bbb")
        `(*TOP*
          "aaa" (*COMMENT* " xxx --") "bbb"))

  (test (html->xexp "aaa<!-- xxx -y-->bbb")
        `(*TOP*
          "aaa" (*COMMENT* " xxx -y") "bbb"))

  (test (html->xexp "aaa<!----->bbb")
        `(*TOP*
          "aaa" (*COMMENT* "-")       "bbb"))

  (test (html->xexp "aaa<!---->bbb")
        `(*TOP*
          "aaa" (*COMMENT* "")        "bbb"))

  (test (html->xexp "aaa<!--->bbb")
        `(*TOP* "aaa" (*COMMENT* "->bbb")))

  (test (html->xexp "<hr>")   `(*TOP* (hr)))
  (test (html->xexp "<hr/>")  `(*TOP* (hr)))
  (test (html->xexp "<hr />") `(*TOP* (hr)))

  (test (html->xexp "<hr noshade>")
        `(*TOP* (hr (@ (noshade)))))
  (test (html->xexp "<hr noshade/>")
        `(*TOP* (hr (@ (noshade)))))
  (test (html->xexp "<hr noshade />")
        `(*TOP* (hr (@ (noshade)))))
  (test (html->xexp "<hr noshade / >")
        `(*TOP* (hr (@ (noshade)))))
  (test (html->xexp "<hr noshade=1 />")
        `(*TOP* (hr (@ (noshade "1")))))
  (test (html->xexp "<hr noshade=1/>")
        `(*TOP* (hr (@ (noshade "1/")))))

  (test (html->xexp "<q>aaa<p/>bbb</q>ccc</p>ddd")
        `(*TOP* (q "aaa" (p) "bbb") "ccc" "ddd"))

  (test (html->xexp "&lt;") `(*TOP* "<"))
  (test (html->xexp "&gt;") `(*TOP* ">"))

  (test (html->xexp "Gilbert &amp; Sullivan")
        `(*TOP* "Gilbert & Sullivan"))
  (test (html->xexp "Gilbert &amp Sullivan")
        `(*TOP* "Gilbert & Sullivan"))
  (test (html->xexp "Gilbert & Sullivan")
        `(*TOP* "Gilbert & Sullivan"))

  (test (html->xexp "Copyright &copy; Foo")
        `(*TOP* "Copyright "
                (& copy)
                " Foo"))
  (test (html->xexp "aaa&copy;bbb")
        `(*TOP*
          "aaa" (& copy) "bbb"))
  
  (test 'unterminated-nonspecial-character-entity-reference-at-eof
        (html->xexp "aaa&copy")
        ;; Note: 20180518 Previously, we parsed this as (*TOP* "aaa" (& copy))
        `(*TOP* "aaa&copy"))

  (test (html->xexp "&#42;")  '(*TOP* "*"))
  (test (html->xexp "&#42")   '(*TOP* "*"))
  (test (html->xexp "&#42x")  '(*TOP* "*x"))
  (test (html->xexp "&#151")  `(*TOP* ,(string (integer->char 151))))
  (test (html->xexp "&#1000") `(*TOP* ,(string (integer->char 1000))))
  (test (html->xexp "&#x42")  '(*TOP* "B"))
  (test (html->xexp "&#xA2")  `(*TOP* ,(string (integer->char 162))))
  (test (html->xexp "&#xFF")  `(*TOP* ,(string (integer->char 255))))
  (test (html->xexp "&#x100") `(*TOP* ,(string (integer->char 256))))
  (test (html->xexp "&#X42")  '(*TOP* "B"))
  (test (html->xexp "&42;")   '(*TOP* "&42;"))

  (test (html->xexp (string-append "aaa&copy;bbb&amp;ccc&lt;ddd&&gt;"
                                   "eee&#42;fff&#1000;ggg&#x5a;hhh"))
        `(*TOP*
          "aaa"
          (& copy)
          ,(string-append "bbb&ccc<ddd&>eee*fff"
                          (string (integer->char 1000))
                          "gggZhhh")))

  (test (html->xexp
         (string-append
          "<IMG src=\"http://e.e/aw/pics/listings/"
          "ebayLogo_38x16.gif\" border=0 width=\"38\" height=\"16\" "
          "HSPACE=5 VSPACE=0\">2</FONT>"))
        `(*TOP*
          (img (@
                (src
                 "http://e.e/aw/pics/listings/ebayLogo_38x16.gif")
                (border "0") (width "38") (height "16")
                (hspace "5") (vspace "0")))
          "2"))

  (test (html->xexp "<aaa bbb=ccc\"ddd>eee")
        `(*TOP* (aaa (@ (bbb "ccc") (ddd)) "eee")))
  (test (html->xexp "<aaa bbb=ccc \"ddd>eee")
        `(*TOP* (aaa (@ (bbb "ccc") (ddd)) "eee")))

  (test (html->xexp
         (string-append
          "<HTML><Head><Title>My Title</Title></Head>"
          "<Body BGColor=\"white\" Foo=42>"
          "This is a <B><I>bold-italic</B></I> test of </Erk>"
          "broken HTML.<br>Yes it is.</Body></HTML>"))
        `(*TOP*
          (html (head (title "My Title"))
                (body (@ (bgcolor "white") (foo "42"))
                      "This is a "
                      (b (i "bold-italic"))
                      " test of "
                      "broken HTML."
                      (br)
                      "Yes it is."))))

  (test (html->xexp
         (string-append
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
          " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
        `(*TOP*
          (*DECL*
           DOCTYPE
           html
           PUBLIC
           "-//W3C//DTD XHTML 1.0 Strict//EN"
           "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")))

  (test (html->xexp
         (string-append
          "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
          "xml:lang=\"en\" "
          "lang=\"en\">"))
        `(*TOP*
          (html (@ (xmlns "http://www.w3.org/1999/xhtml")
                   (xml:lang "en") (lang "en")))))

  (test (html->xexp
         (string-append
          "<html:html xmlns:html=\"http://www.w3.org/TR/REC-html40\">"
          "<html:head><html:title>Frobnostication</html:title></html:head>"
          "<html:body><html:p>Moved to <html:a href=\"http://frob.com\">"
          "here.</html:a></html:p></html:body></html:html>"))
        `(*TOP*
          (html (@ (xmlns:html "http://www.w3.org/TR/REC-html40"))
                (head (title "Frobnostication"))
                (body (p "Moved to "
                         (a (@ (href "http://frob.com"))
                            "here."))))))

  (test (html->xexp
         (string-append
          "<RESERVATION xmlns:HTML=\"http://www.w3.org/TR/REC-html40\">"
          "<NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
          "<SEAT CLASS=\"Y\" HTML:CLASS=\"largeMonotype\">33B</SEAT>"
          "<HTML:A HREF=\"/cgi-bin/ResStatus\">Check Status</HTML:A>"
          "<DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>"))
        `(*TOP*
          (reservation (@ (,(string->symbol "xmlns:HTML")
                           "http://www.w3.org/TR/REC-html40"))
                       (name (@ (class "largeSansSerif"))
                             "Layman, A")
                       (seat (@ (class "Y") (class "largeMonotype"))
                             "33B")
                       (a (@ (href "/cgi-bin/ResStatus"))
                          "Check Status")
                       (departure "1997-05-24T07:55:00+1"))))

  (test (html->xexp
         (string-append
          "<html><head><title></title><title>whatever</title></head><body>"
          "<a href=\"url\">link</a><p align=center><ul compact style=\"aa\">"
          "<p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened </i>"
          " still &lt; bold </b></body><P> But not done yet..."))
        `(*TOP*
          (html (head (title) (title "whatever"))
                (body (a (@ (href "url")) "link")
                      (p (@ (align "center"))
                         (ul (@ (compact) (style "aa"))))
                      (p "BLah"
                         (*COMMENT* " comment <comment> ")
                         " "
                         (i " italic " (b " bold " (tt " ened ")))
                         " still < bold "))
                (p " But not done yet..."))))

  (test (html->xexp "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
        `(*TOP*
          (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")))

  (test (html->xexp "<?php php_info(); ?>")
        `(*TOP* (*PI* php "php_info(); ")))
  (test (html->xexp "<?php php_info(); ?")
        `(*TOP* (*PI* php "php_info(); ?")))
  (test (html->xexp "<?php php_info(); ")
        `(*TOP* (*PI* php "php_info(); ")))

  (test (html->xexp "<?foo bar ? baz > blort ?>")
        `(*TOP*
          (*PI* foo "bar ? baz > blort ")))

  (test (html->xexp "<?foo b?>x")
        `(*TOP* (*PI* foo "b") "x"))
  (test (html->xexp "<?foo ?>x")
        `(*TOP* (*PI* foo "")  "x"))
  (test (html->xexp "<?foo ?>x")
        `(*TOP* (*PI* foo "")  "x"))
  (test (html->xexp "<?foo?>x")
        `(*TOP* (*PI* foo "")  "x"))
  (test (html->xexp "<?f?>x")
        `(*TOP* (*PI* f   "")  "x"))
  (test (html->xexp "<??>x")
        `(*TOP* (*PI* #f  "")  "x"))
  (test (html->xexp "<?>x")
        `(*TOP* (*PI* #f  ">x")))

  (test (html->xexp "<foo bar=\"baz\">blort")
        `(*TOP* (foo (@ (bar "baz")) "blort")))
  (test (html->xexp "<foo bar='baz'>blort")
        `(*TOP* (foo (@ (bar "baz")) "blort")))
  (test (html->xexp "<foo bar=\"baz'>blort")
        `(*TOP* (foo (@ (bar "baz'>blort")))))
  (test (html->xexp "<foo bar='baz\">blort")
        `(*TOP* (foo (@ (bar "baz\">blort")))))

  (test (html->xexp (string-append "<p>A</p>"
                                   "<script>line0 <" "\n"
                                   "line1" "\n"
                                   "<line2></script>"
                                   "<p>B</p>"))
        `(*TOP* (p "A")
                (script ,(string-append "line0 <" "\n")
                        ,(string-append "line1"   "\n")
                        "<line2>")
                (p "B")))

  (test (html->xexp "<xmp>a<b>c</XMP>d")
        `(*TOP* (xmp "a<b>c") "d"))
  (test (html->xexp "<XMP>a<b>c</xmp>d")
        `(*TOP* (xmp "a<b>c") "d"))
  (test (html->xexp "<xmp>a<b>c</foo:xmp>d")
        `(*TOP* (xmp "a<b>c") "d"))
  (test (html->xexp "<foo:xmp>a<b>c</xmp>d")
        `(*TOP* (xmp "a<b>c") "d"))
  (test (html->xexp "<foo:xmp>a<b>c</foo:xmp>d")
        `(*TOP* (xmp "a<b>c") "d"))
  (test (html->xexp "<foo:xmp>a<b>c</bar:xmp>d")
        `(*TOP* (xmp "a<b>c") "d"))

  (test (html->xexp "<xmp>a</b>c</xmp>d")
        `(*TOP* (xmp "a</b>c")     "d"))
  (test (html->xexp "<xmp>a</b >c</xmp>d")
        `(*TOP* (xmp "a</b >c")    "d"))
  (test (html->xexp "<xmp>a</ b>c</xmp>d")
        `(*TOP* (xmp "a</ b>c")    "d"))
  (test (html->xexp "<xmp>a</ b >c</xmp>d")
        `(*TOP* (xmp "a</ b >c")   "d"))
  (test (html->xexp "<xmp>a</b:x>c</xmp>d")
        `(*TOP* (xmp "a</b:x>c")   "d"))
  (test (html->xexp "<xmp>a</b::x>c</xmp>d")
        `(*TOP* (xmp "a</b::x>c")  "d"))
  (test (html->xexp "<xmp>a</b:::x>c</xmp>d")
        `(*TOP* (xmp "a</b:::x>c") "d"))
  (test (html->xexp "<xmp>a</b:>c</xmp>d")
        `(*TOP* (xmp "a</b:>c")    "d"))
  (test (html->xexp "<xmp>a</b::>c</xmp>d")
        `(*TOP* (xmp "a</b::>c")   "d"))
  (test (html->xexp "<xmp>a</xmp:b>c</xmp>d")
        `(*TOP* (xmp "a</xmp:b>c") "d"))

  (let ((expected `(*TOP* (p "real1")
                          "\n"
                          (xmp "\n"
                               ,(string-append "alpha"       "\n")
                               ,(string-append "<P>fake</P>" "\n")
                               ,(string-append "bravo"       "\n"))
                          (p "real2"))))

    (test (html->xexp (string-append "<P>real1</P>" "\n"
                                     "<XMP>"        "\n"
                                     "alpha"        "\n"
                                     "<P>fake</P>"  "\n"
                                     "bravo"        "\n"
                                     "</XMP "       "\n"
                                     "<P>real2</P>"))
          expected)

    (test (html->xexp (string-append "<P>real1</P>" "\n"
                                     "<XMP>"        "\n"
                                     "alpha"        "\n"
                                     "<P>fake</P>"  "\n"
                                     "bravo"        "\n"
                                     "</XMP"        "\n"
                                     "<P>real2</P>"))
          expected))

  (test (html->xexp "<xmp>a</xmp>x")
        `(*TOP* (xmp "a")   "x"))
  (test (html->xexp (string-append "<xmp>a" "\n" "</xmp>x"))
        `(*TOP* (xmp ,(string-append "a" "\n")) "x"))
  (test (html->xexp "<xmp></xmp>x")
        `(*TOP* (xmp)       "x"))

  (test (html->xexp "<xmp>a</xmp") `(*TOP* (xmp "a")))
  (test (html->xexp "<xmp>a</xm")  `(*TOP* (xmp "a</xm")))
  (test (html->xexp "<xmp>a</x")   `(*TOP* (xmp "a</x")))
  (test (html->xexp "<xmp>a</")    `(*TOP* (xmp "a</")))
  (test (html->xexp "<xmp>a<")     `(*TOP* (xmp "a<")))
  (test (html->xexp "<xmp>a")      `(*TOP* (xmp "a")))
  (test (html->xexp "<xmp>")       `(*TOP* (xmp)))
  (test (html->xexp "<xmp")        `(*TOP* (xmp)))

  (test (html->xexp "<xmp x=42 ")
        `(*TOP* (xmp (@ (x "42")))))
  (test (html->xexp "<xmp x= ")   `(*TOP* (xmp (@ (x)))))
  (test (html->xexp "<xmp x ")    `(*TOP* (xmp (@ (x)))))
  (test (html->xexp "<xmp x")     `(*TOP* (xmp (@ (x)))))

  (test (html->xexp "<script>xxx")
        `(*TOP* (script "xxx")))
  (test (html->xexp "<script/>xxx")
        `(*TOP* (script) "xxx"))

  (test (html->xexp "<html xml:lang=\"en\" lang=\"en\">")
        `(*TOP* (html (@ (xml:lang "en") (lang "en")))))

  (test (html->xexp "<a href=/foo.html>")
        `(*TOP* (a (@ (href "/foo.html")))))
  (test (html->xexp "<a href=/>foo.html")
        `(*TOP* (a (@ (href "/")) "foo.html")))

  ;; TODO: Add verbatim-pair cases with attributes in the end tag.

  (test (html->xexp "&copy;")
        `(*TOP* (& copy)))
  (test (html->xexp "&rArr;")
        `(*TOP* (& ,(string->symbol "rArr"))))
  (test (html->xexp "&#151;")
        `(*TOP* ,(string (integer->char 151))))

  (test (html->xexp "&#999;")
        `(*TOP* ,(string (integer->char 999))))

  (test (html->xexp "xxx<![CDATA[abc]]>yyy")
        `(*TOP* "xxx" "abc" "yyy"))

  (test (html->xexp "xxx<![CDATA[ab]c]]>yyy")
        `(*TOP* "xxx" "ab]c" "yyy"))

  (test (html->xexp "xxx<![CDATA[ab]]c]]>yyy")
        `(*TOP* "xxx" "ab]]c" "yyy"))

  (test (html->xexp "xxx<![CDATA[]]]>yyy")
        `(*TOP* "xxx" "]" "yyy"))

  (test (html->xexp "xxx<![CDATAyyy")
        `(*TOP* "xxx" "<![CDATA" "yyy"))

  (test (html->xexp "<html><div><p>P1</p><p>P2</p></div><p>P3</p>")
        `(*TOP* (html (div (p "P1")
                           (p "P2"))
                      (p "P3")))
        #:id 'parent-constraints-with-div)

  (test (html->xexp "&#151;")
        `(*TOP* ,(string (integer->char 151)))
        #:id 'once-again-converting-character-references-above-126-to-string)

  (test (html->xexp "<ul><li>a<p>b</p>")
        `(*TOP* (ul (li "a" (p "b"))))
        #:id 'p-element-can-be-child-of-li-element)

  (test-section 'unterminated-named-character-entity-references

    (test-section 'old-school-names
      (test 'amp  (html->xexp "<p>a&ampz</p>")  '(*TOP* (p "a&z")))
      (test 'apos (html->xexp "<p>a&aposz</p>") '(*TOP* (p "a'z")))
      (test 'lt   (html->xexp "<p>a&ltz</p>")   '(*TOP* (p "a<z")))
      (test 'gt   (html->xexp "<p>a&gtz</p>")   '(*TOP* (p "a>z")))
      (test 'quot (html->xexp "<p>a&quotz</p>") '(*TOP* (p "a\"z"))))

    (test-section 'new-school-names
      (test 'rarr (html->xexp "<p>a&rarrz</p>") '(*TOP* (p "a&rarrz"))))

    (test-section 'unrecognized-names
      (test 'a-t-and-t
            (html->xexp "<p>AT&T Bell Labs</p>")
            '(*TOP* (p "AT&T Bell Labs")))))
  
  (test-section 'terminated-old-school-named-character-entity-references
    (test 'amp  (html->xexp "<p>a&amp;z</p>")  '(*TOP* (p "a&z")))
    (test 'apos (html->xexp "<p>a&apos;z</p>") '(*TOP* (p "a'z")))
    (test 'lt   (html->xexp "<p>a&lt;z</p>")   '(*TOP* (p "a<z")))
    (test 'gt   (html->xexp "<p>a&gt;z</p>")   '(*TOP* (p "a>z")))
    (test 'quot (html->xexp "<p>a&quot;z</p>") '(*TOP* (p "a\"z"))))

  (test-section 'parent-constraints-should-permit-p-inside-blockquote
    (test 'basic
          (html->xexp "<x>A<blockquote><p>B</p>C<p>D</p>E</blockquote>F</x>")
          `(*TOP* (x "A"
                     (blockquote (p "B")
                                 "C"
                                 (p "D")
                                 "E")
                     "F")))
    (test 'initial-sorawee-porncharoenwase-example
          (html->xexp 
           "<blockquote><tr><td><blockquote><p></p></blockquote></td><div></div></tr></blockquote>")
          '(*TOP* (blockquote (tr (td (blockquote (p)))
                                  (div))))))

  (test-section 'p-elem-can-be-child-of-details-elem

    (test 'initial-jacder-example-modified
          (html->xexp "<div><details><p>text in details</p></details></div>")
          '(*TOP* (div (details (p "text in details"))))))

  ;; TODO: 2022-01-22 Go through latest HTML elements and add them to parent constraints
  ;; in the ancient quirks handling from over 20 years ago.

  (test-section 'p-elem-cannot-be-child-of-p-elem

    (test 'initial-simpson-example
          (html->xexp "<html><p>foo<p>bar</html>")
          '(*TOP* (html (p "foo") (p "bar"))))

    (test 'non-p-parent-in-which-p-shouldnt-nest
          (html->xexp "<html><xyz>foo<p>bar</html>")
          '(*TOP* (html (xyz "foo") (p "bar"))))

    (test 'p-shouldnt-nest-even-without-html-element
          (html->xexp "<p>foo<p>bar")
          '(*TOP* (p "foo") (p "bar"))
          #:fail "see code comment about parent-constraints when no top elements"))

  (test-section 'break-the-world-with-h-elem-mid-90s-handling

    (test 'initial
          (html->xexp "<html><p>foo<h3>bar</h3></html>")
          '(*TOP* (html (p "foo") (h3 "bar")))))

  (test-section 'twrpme-h2-in-li-elements

    (test 'simple
          (html->xexp "<html><body><ul><li><h2>My Header Item</h2></li><li>My Non-Header Item</li></ul></body></html>")
          '(*TOP* (html (body (ul (li (h2 "My Header Item"))
                                  (li "My Non-Header Item"))))))

    (test 'simon-budinsky-example
          (html->xexp
           (string-append
            "<html>"
            "<head></head>"
            "<body>"
            "<ul class=\"post-list\">"
            "<li>"
            "<span class=\"post-meta\">Mar 10, 2022</span>"
            "<h2>"
            "<a class=\"post-link\" href=\"/site/update/2022/03/10/twrp-3.6.1-released.html\">TWRP 3.6.1 Released</a>"
            "</h2>"
            "</li>"
            "<li>"
            "<span class=\"post-meta\">Mar 10, 2022</span>"
            "<h2>"
            "<a class=\"post-link\" href=\"/site/update/2022/03/10/twrp-3.6.1-released.html\">TWRP 3.6.1 Released</a>"
            "</h2>"
            "</li>"
            "</ul>"
            "</body>"
            "</html>"))
          '(*TOP* (html (head)
                        (body (ul (@ (class "post-list"))
                                  (li (span (@ (class "post-meta"))
                                            "Mar 10, 2022")
                                      (h2 (a (@ (class "post-link")
                                                (href "/site/update/2022/03/10/twrp-3.6.1-released.html"))
                                             "TWRP 3.6.1 Released")))
                                  
                                  (li (span (@ (class "post-meta"))
                                            "Mar 10, 2022")
                                      (h2 (a (@ (class "post-link")
                                                (href "/site/update/2022/03/10/twrp-3.6.1-released.html"))
                                             "TWRP 3.6.1 Released")))))))))

  (test-section 'area-in-span-in-microformats

    (test 'area-from-jacob-hall
          (html->xexp "<div><span><area></area></span></div>")
          '(*TOP* (div (span (area)))))

    (test 'object-from-jacob-hall
          (html->xexp "<div><object>Jane Doe</object></div>")
          '(*TOP* (div (object "Jane Doe")))))

  ;; TODO: Document this.
  ;;
  ;; (define html-1 "<myelem myattr=\"&\">")
  ;; (define shtml   (html->xexp html-1))
  ;; shtml
  ;; (define html-2 (shtml->html shtml))
  ;; html-2
  )

(doc history

     (#:planet 11:0 #:date "2022-07-19"
               (itemlist
                (item "An "
                      (code "object")
                      " element is no longer considered always-empty.  Incrementing major
version again, because this could break parses.")))
     
     (#:planet 10:0 #:date "2022-07-19"
               (itemlist
                (item "To pass a \"microformats\" test suite ("
                      (hyperlink "https://github.com/microformats/tests/blob/master/tests/microformats-v2/h-card/impliedname.html#L11"
                                 "impliedname.html")
                      "), an "
                      (code "area")
                      " element can now be a child of a "
                      (code "span")
                      " element.  In the future, we might be even more flexible about where "
                      (code "span")
                      " elements are permitted.  (Thanks to Jacob Hall for
discussing.)")))
     
     (#:planet 9:0 #:date "2022-04-16"
               (itemlist
                (item "Header elements may once again appear as children of "
                      (code "li")
                      " elements (which we broke in the previous version), as we see how far we can stretch a 20 year-old hack for invalid HTML.  (Thanks for Simon Budinsky for reporting.)")))

     (#:planet 8:0 #:date "2022-04-03"
               (itemlist
                (item "The original \"H\" elements ("
                      (code "h1")
                      ", "
                      (code "h2")
                      ", etc.) now are parsed with \"parent constraints\" for
handling invalid HTML, to accommodate a need to parse mid-1990s HTML in which "
                      (code "<P>")
                      " was used as a separator or terminator, rather than a
start delimeter.  There is a chance that change this will break a real-world
scraper or other tool.")))
                
     (#:planet 7:1 #:date "2022-04-02"
               (itemlist
                (item "Include a test case #:fail that was unsaved in DrRacket.")))
     
     (#:planet 7:0 #:date "2022-04-02"
               (itemlist
                
                (item "Fixed a quirks-handling bug in which "
                      (code "p")
                      " elements would be (directly or indirectly) nested under other "
                      (code "p")
                      " elements in cases in which there was no "
                      (code "body")
                      " element, but there was an "
                      (code "html")
                      " element.  (Thanks to Jonathan Simpson for reporting.)")))
     
     (#:planet 6:1 #:date "2022-01-22"
               (itemlist
                (item "Permit "
                      (code "details")
                      " element to be parent of "
                      (code "p")
                      " element in quirks handling. (Thanks to Jacder for reporting.)")))
     
     (#:planet 6:0 #:date "2018-05-22"
               (itemlist
                (item "Fix to permit "
                      (code "p")
                      " elements as children of "
                      (code "blockquote")
                      " elements.  Incrementing major version number because
this is a breaking change of 17 years, but seems an appropriate change for
modern HTML, and fixes a particular real-world problem.  (Thanks to
Sorawee Porncharoenwase for reporting.)")))
     
     (#:planet 5:0 #:date "2018-05-15"
               (itemlist
                (item "In a breaking change of handing invalid HTML, most named
character entity references that are invalid because (possibly among multiple
reasons) they are not terminated by semicolon, now are treated as literal
strings (including the ampersand), rather than as named character entites.  For
example, parser input string "
                      (racket "<p>A&B Co.<p>")
                      " will now parse as "
                      (racket (p "A&B Co."))
                      " rather than as "
                      (racket (p "A" (& B) " Co."))
                      ".  (Thanks for Greg Hendershott for suggesting this, and discussing.)")
                (item "For support of historical quirks handling, five early
HTML named character entities (specifically, "
                      (tt "amp")
                      ", "
                      (tt "apos")
                      ", "
                      (tt "lt")
                      ", "
                      (tt "gt")
                      ", "
                      (tt "quot")
                      ") do not need to be terminated with a semicolon, and
will even be recognized if followed immediately by an alphabetic.  For
example, "
                      (racket "<p>a&ltz</p>")
                      " will now parse as "
                      (racket (p "a<z"))
                      ", rather than as "
                      (racket (p (& ltz)))
                      ".")
                (item "Invalid character entity references that are terminated
by EOF rather than semicolon may now be parsed as literal strings, rather than
as entity references.")))
               
     (#:planet 4:3 #:date "2016-12-15"
               (itemlist
                (item "Error message ``"
                      (code "%html-parsing:parse-html: invalid input type:")
                      "'' now abbreviates the invalid value, to avoid possibly
huge messages.  (Thanks to John B. Clements.)")))

     (#:planet 4:2 #:date "2016-03-02"
               (itemlist
                (item "Tweaked info.rkt, filenames.")))
     
     (#:planet 4:1 #:date "2016-02-25"
               (itemlist
                (item "Updated deps.")
                (item "Documentation tweaks.")))

     (#:planet 4:0 #:date "2016-02-21"
               (itemlist
                (item "Moving from PLaneT to new package system.")
                (item "Moved unit tests into main source file.")))

     (#:planet 3:0 #:date "2015-04-24"
               (itemlist
                (item "Numeric character entities now parse to Racket strings
instead of Racket characters, to bring SXML/xexp back closer to SXML.  (Thanks
to John Clements for reporting.)")))

     (#:planet 2:0 #:date "2012-06-13"
               (itemlist
                (item "Converted to McFly.")))

     (#:version "0.3" #:planet 1:2 #:date "2011-08-27"
                (itemlist
                 (item "Converted test suite from Testeez to Overeasy.")))

     (#:version "0.2" #:planet 1:1 #:date "2011-08-27"
                (itemlist
                 (item "Fixed embarrassing bug due to code tidying.  (Thanks to
Danny Yoo for reporting.)")))

     (#:version "0.1" #:planet 1:0 #:date "2011-08-21"
                (itemlist
                 (item "Part of forked development from HtmlPrag, parser
originally written 2001-04."))))
