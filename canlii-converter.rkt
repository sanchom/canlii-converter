#lang racket

(require net/url)

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

(define/contract (fetch-from-canlii-cached URL)
  (string? . -> . string?)
  (define cached-filename (docname (stripped-canlii-url URL)))
  (if (file-exists? cached-filename)
      (let* ([in (open-input-file cached-filename)])
        (port->string in)) ; Also closes the port
      (let* ([out (open-output-file cached-filename)]
             [content (fetch-from-canlii URL)])
        (write-string content out) ; Does not close the port
        (close-output-port out) ; Closes the port
        content)))

(define (parse-statute URL)
  (define content (fetch-from-canlii-cached URL))
  "got content")