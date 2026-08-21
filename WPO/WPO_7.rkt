#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d queue list) queue:))

; H4 start

; Oef 3
(define (josephus lst m)
  (define queue (queue:new))
  
  (define (iter lst)
    (when (not (null? lst))
      (queue:enqueue! queue (car lst))
      (iter (cdr lst))))
  (iter lst)
   
  (let loop ((i 1))
    (let ((element (queue:serve! queue)))
      (cond ((queue:empty? queue) element)
            ((= i m) (loop 1))
            (else
             (queue:enqueue! queue element)
             (loop (+ i 1)))))))


(display (josephus '(a b c d e) 3))
