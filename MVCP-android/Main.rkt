#lang racket

(require racket/cmdline)
(require racket/path)

(define (current-d [path (current-directory)])
  (map path->string (directory-list path)))

(define (is-android dirlist) (foldl (lambda (a result)
                            (or (string-suffix? a ".iml") result))
                          #f dirlist))

(define child-directory
  (filter directory-exists? (directory-list)))

(define currdir (path->string (current-directory)))


(define cd 
  (begin
    (display "This program will produce a template for the MVC pattern on our Android project\n")
    (printf "Type in the path to the Android project (default is ~a):" currdir)
    (let* ([homedir (path->string (find-system-path 'home-dir))]
           [r (string-replace (read-line) "~" homedir)])
    (string->path (if (non-empty-string? r) r currdir)))))
(if (is-android (current-d cd)) #t #f)