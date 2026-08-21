#lang r7rs
(import (scheme base)
        (scheme write)
        (racket/trace)
        (prefix (a-d positional-list double-linked-positional-list) plist:)
        (prefix (a-d sorted-list vectorial-Copy) slist:)
        (prefix (a-d sorted-list linked-Copy) slist_extra:)
        (a-d pattern-matching quicksearch))

; H3

; Oef 5
(define (intersection1 pl1 pl2)
  (define result (plist:new eq?))
  (plist:for-each pl1 (lambda (el)
                        (if (plist:find pl2 el)
                            (plist:add-after! result el))))
  result)

(define (intersection2 pl1 pl2)
  (define result (plist:new eq?))
  (let loop ((i (plist:first pl1)))  
    (let ((value (plist:peek pl1 i)))s
      (if (plist:find pl2 value)
        (plist:add-after! result value)))
    (if (plist:has-next? pl1 i)
      (loop (plist:next pl1 i))))
  result)

(define plist1 (plist:from-scheme-list '(2 4 5 6 7 8) =))
(define plist2 (plist:from-scheme-list '(1 3 5 7 9) =))
(display (intersection1 plist1 plist2))
(newline)
(display (intersection1 plist1 plist2))

; Oef 7
; zie Aglo 3/r7rs-library/a-d/sorted-list/vectorial-Copy.rkt voor oplossing
(newline)
(display (slist:find-ternary! (slist:from-scheme-list (list 1 2 3 5 4 6 7 8 9) < =) 9))

; Oef 8
; zie Aglo 3/r7rs-library/a-d/sorted-list/linked-Copy.rkt voor oplossing
