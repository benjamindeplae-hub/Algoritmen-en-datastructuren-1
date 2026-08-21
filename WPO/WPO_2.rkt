#lang r7rs
(import (scheme base))

(define (last-of-vector v)
  (vector-ref v (- (vector-length v) 1)))
; worst: O(1)
; best: Omega(1)
; theta(1)

(define (last-of-list l)
  (list-ref l (- (length l) 1)))
; worst: O(n)
; best: Omega(n)
; theta(n)