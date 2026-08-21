#lang r7rs
(import (scheme base)
        (scheme write)
        (prefix (a-d ring) ring:)
        (prefix (H1 fraction) frac:))

(define f1 (frac:new 1 2))
(define f2 (frac:new 2 3))
(frac:+ f1 f2)

(define (frac:= frac1 frac2)
  (= (/ (frac:numer frac1) (frac:denom frac1))
     (/ (frac:numer frac2) (frac:denom frac2))))

(frac:= f1 (frac:new 2 4))

(define f3 (frac:new 2 4))

(define f4 (frac:new 2 0))