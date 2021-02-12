#lang racket

(require net/url)
(require html-parsing)
(require txexpr)
(require xml)
(require sxml)
(require pollen/decode)

(define/contract (stripped-canlii-url URL)
  (string? . -> . string?)
  (first (string-split URL "?")))

(define/contract (docname stripped-URL)
  ((and/c string? (lambda (x) (not (string-contains? x "?")))) . -> . string?)
  (define-values (a b c) (split-path stripped-URL))
  (path->string b))

(define (fetch-from-canlii URL)
  (call/input-url (string->url URL)
                  get-pure-port
                  port->string)) ; Also closes the port

(define/contract (convert-canlii-html-to-txexpr content)
  (string? . -> . txexpr?)
  ; This goes through several converters to get to the form I want to use.
  ;
  ; html->xexp is a permissive parser than can handle typical non-standard html.
  ;
  ; But, it produces sxml, which doesn't have the nice traversal functions
  ; that the txexpr library does.
  ;
  ; So I convert the sxml to a string and then back into a txexpr.
  (string->xexpr (srl:sxml->xml (html->xexp content))))

(define/contract (fetch-from-canlii-cached URL)
  (string? . -> . txexpr?)
  (define cached-filename (docname (stripped-canlii-url URL)))
  (if (file-exists? cached-filename)
      (let* ([in (open-input-file cached-filename)])
        (convert-canlii-html-to-txexpr (port->string in))) ; Also closes the port
      (let* ([out (open-output-file cached-filename)]
             [content (fetch-from-canlii URL)])
        (write-string content out) ; Does not close the port
        (close-output-port out) ; Closes the port
        (convert-canlii-html-to-txexpr content))))

(define/contract (decode-section section-tx)
  (txexpr? . -> . txexpr?)
  
  (define (strip-invisible-links tx)
    (define (is-invisible-link? x)
      (and (txexpr? x) (eq? 'a (get-tag x)) (empty? (get-elements x))))
    (if (is-invisible-link? tx) '() tx))

  (define (convert-section-title tx)
    (define (is-section-title? x)
      (and (txexpr? x) (eq? 'h4 (get-tag x))))
    (if (is-section-title? tx) `(div [(class "section-heading")] ,@(get-elements tx)) tx))

  (define (strip-pure-whitespace el)
    (if (regexp-match-exact? #px"(\\s| )*" el) '() el)) ; that's a non-breaking space in there.
  
  (define cleaned-section (decode section-tx
                                  #:txexpr-proc (compose1 strip-invisible-links convert-section-title)
                                  #:string-proc strip-pure-whitespace))
  (define (get-heading tx)
    (findf-txexpr tx (lambda (x) (and (txexpr? x) (equal? "section-heading" (attr-ref x 'class ""))))))

  (define (get-section-number tx)
    "3")

  (define (has-paragraphs? tx) #t)

  (define/contract (separate-into-subsections tx)
    (txexpr? . -> . (listof txexpr?))
    
  
  (define/contract (parse-subsections tx)
    (txexpr? . -> . txexpr?)
    (define subsections (separate-into-subsections tx))
    `(ol [[class "subsections"]]
         ,@(map (lambda (x) (if (has-paras? x) (parse-paras x) (parse-standalone-subsection x))) subsections)))
    
  (define (get-section-content tx)
    `(div [[class "section-content"]]
          (div [[class "section-number"]] ,(get-section-number tx))
          (if (has-subsections? tx) (parse-subsections tx) (parse-standalone-section tx))))
  
  `(div [[class "section"]]
        ,(get-heading cleaned-section)
        ,(get-section-content cleaned-section)))

(define (parse-statute URL)
  (define content (fetch-from-canlii-cached URL))
  
  (define (is-main-content? x)
    (and (txexpr? x) (eq? 'div (get-tag x)) (equal? "canliidocumentcontent" (attr-ref x 'class ""))))

  (define (is-section? x)
    (and (txexpr? x) (eq? 'div (get-tag x)) (equal? "section" (attr-ref x 'class ""))))
  
  (define main-content (findf-txexpr content is-main-content?))
  (define section-list (findf*-txexpr main-content is-section?))

  (third (map decode-section section-list)))