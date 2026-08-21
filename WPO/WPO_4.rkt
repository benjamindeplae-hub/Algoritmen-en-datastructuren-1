#lang r7rs
(import (scheme base)
        (scheme write)
        (racket/trace)
        (prefix (a-d positional-list double-linked-positional-list) plist:)
        (a-d pattern-matching quicksearch))


; H3 Exercise


; Oef 1c
(define-record-type exercise-c
  (new a b c)
  exercise-c?
  (a get-a set-a!)
  (b get-b set-b!)
  (c get-c set-c!))

(define (ex-c)
  (let ((the-list (list -5 -8 -1 6 2 0))
        (the-last-cell (list 7)))
    (new the-last-cell 3 (append the-list the-last-cell)))) ; of list-tail op length - 1 voor het laatste element te krijgen en in een list te zetten

(define instance-ex-c (ex-c))
(display instance-ex-c)
(newline)
(set-car! (get-a instance-ex-c) 45)
(display instance-ex-c)
(newline)

; Oef 2
(define my-plist (plist:new string=?))
(plist:add-before! my-plist "and")
(plist:add-after! my-plist "me")
(plist:add-after! my-plist "to" (plist:first my-plist))
(plist:add-after! my-plist "goodday" (plist:first my-plist))
(plist:add-before! my-plist "hello")
(plist:add-after! my-plist "world" (plist:first my-plist))

(define (plist-display l)
  (plist:for-each l (lambda (element)
                      (display element)
                      (display #\space)))
  (newline))
(plist-display my-plist)

(define (count-words-with-e-plist l)
  (define counter 0)
  (plist:for-each l (lambda (el)
                      (if (match el "e")
                          (set! counter (+ counter 1)))))
  counter)
(display (count-words-with-e-plist my-plist))

(newline)
; Oef 3
(define (pair-eq? l1 l2)
  (eq? (cdr l1)
       (cdr l2)))

(define my-plist-map (plist:map my-plist (lambda (el)
           (cons el (string-length el)))
       pair-eq?))

(display (plist:find my-plist-map (cons 'doesnotmatter 7)))

; Oef 4
; see double-linked-positional-list-Copy.rkt in same directory, not corrected??

