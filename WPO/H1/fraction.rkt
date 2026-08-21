#lang r7rs
(define-library (fraction)
  (export new numer denom fraction? + - * /)
  (import (rename(scheme base)
                 (+ base:+)
                 (- base:-)
                 (* base:*)
                 (/ base:/))
          (scheme write))
  (begin

    ; new: (number number -> fraction)
    ; numer: (fraction -> number)
    ; denom: (fraction -> number)
    ; fraction? (any -> boolean)
    ; +, -, *, / : (fraction fraction -> fraction)

    (define-record-type fraction
      (make-frac n d)
      fraction?
      (n numer numer!)
      (d denom denom!))

    (define (new n d)
      (if (= d 0)  ; Check if the denominator is zero
          (error "Denominator cannot be zero")  ; Throw an error if the denominator is zero
          (let ((greatest-common-divisioner (gcd n d)))
            (make-frac (base:/ n greatest-common-divisioner)
                       (base:/ d greatest-common-divisioner)))))

    (define (+ frac1 frac2)
      (new (base:+ (base:* (numer frac1) (denom frac2))
                   (base:* (numer frac2) (denom frac1)))
           (base:* (denom frac1)
                   (denom frac2))))

    (define (- frac1 frac2)
      (new (base:- (base:* (numer frac1) (denom frac2))
                   (base:* (numer frac2) (denom frac1)))
           (base:* (denom frac1)
                   (denom frac2))))

    (define (* frac1 frac2)
      (new (base:* (numer frac1) (numer frac2))
           (base:* (denom frac1) (denom frac2))))

    (define (/ frac1 frac2)
      (new (base:* (numer frac1) (denom frac2))
           (base:* (denom frac1) (numer frac2))))))