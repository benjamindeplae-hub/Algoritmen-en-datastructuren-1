#lang r7rs

; H5

(define-library (sorting)
  (export bubble-sort insertion-sort)
  (import (scheme base)
          (scheme write))
  (begin
    
    (define (bubble-sort lst <<?)  
      (define (bubble-swap cons1 cons2)
        (let ((keep (car cons1)))
          (set-car! cons1 (car cons2))
          (set-car! cons2 keep)
          #t))
      (let outer-loop
        ((unsorted-idx (- (length lst) 2)))
        (if (>= unsorted-idx 0)
            (if (let inner-loop
                  ((inner-idx 0)
                   (has-changed? #f)
                   (current lst))
                  (if (> inner-idx unsorted-idx)
                      has-changed?
                      (inner-loop (+ inner-idx 1)
                                  (if (<<? (cadr current)
                                           (car current))
                                      (bubble-swap current (cdr current))
                                      has-changed?)
                                  (cdr current))))
                (outer-loop (- unsorted-idx 1))))))

    (define (insertion-sort vector <<?)
      (define (>=? x y) (not (<<? x y)))
      (let outer-loop
        ((outer-idx (- (vector-length vector) 2)))
        (let
            ((current (vector-ref vector outer-idx)))
          (vector-set! 
           vector 
           (let inner-loop
             ((inner-idx (+ 1 outer-idx)))
             (cond
               ((or (>= inner-idx (vector-length vector))
                    (>=? (vector-ref vector inner-idx)
                         current))
                (- inner-idx 1))
               (else
                (vector-set! vector (- inner-idx 1) (vector-ref vector inner-idx))
                (inner-loop (+ inner-idx 1)))))
           current)
          (if (> outer-idx 0)
              (outer-loop (- outer-idx 1))))))

    (define (selection-sort vector <<?)
      (define index-vector
        (let ((n (vector-length vector)))
          (let ((result (make-vector n)))
            (do ((i 0 (+ i 1)))
              ((= i n) result)
              (vector-set! result i i)))))

      (define (swap vector i j)
        (let ((keep (vector-ref vector i)))
          (vector-set! vector i (vector-ref vector j))
          (vector-set! vector j keep)))
      (let outer-loop
        ((outer-idx 0))
        (swap index-vector
              outer-idx 
              (let inner-loop
                ((inner-idx (+ outer-idx 1))
                 (smallest-idx outer-idx))
                (cond 
                  ((>= inner-idx (vector-length vector))
                   smallest-idx)
                  ((<<? (vector-ref vector (vector-ref index-vector inner-idx))
                        (vector-ref vector (vector-ref index-vector smallest-idx)))
                   (inner-loop (+ inner-idx 1) inner-idx))
                  (else
                   (inner-loop (+ inner-idx 1) smallest-idx)))))
        (if (< outer-idx (- (vector-length vector) 1))
            (outer-loop (+ outer-idx 1))
            index-vector)))

    (define (mergesort vector <<?)
      (define (merge vector p q r)
        (let ((working-vector (make-vector (+ (- r p) 1))))
          (define (copy-back a b)
            (vector-set! vector b (vector-ref working-vector a))
            (if (< a (- (vector-length working-vector) 1))
                (copy-back (+ a 1) (+ b 1))))
          (define (flush-remaining k i until)
            (vector-set! working-vector k (vector-ref vector i))
            (if (< i until)
                (flush-remaining (+ k 1) (+ i 1) until)
                (copy-back 0 p)))
          (define (merge-iter k i j)
            (cond ((and (<= i q) (<= j r))
                   (let ((low1 (vector-ref vector i))
                         (low2 (vector-ref vector j)))
                     (if (<<? low1 low2)
                         (begin 
                           (vector-set! working-vector k low1)
                           (merge-iter (+ k 1) (+ i 1) j))
                         (begin 
                           (vector-set! working-vector k low2)
                           (merge-iter (+ k 1) i (+ j 1))))))
                  ((<= i q)
                   (flush-remaining k i q))
                  (else
                   (flush-remaining k j r))))
          (merge-iter 0 p (+ q 1))))
      (define (merge-sort-rec vector p r)
        (if (< p r)
            (let ((q (quotient (+ r p) 2)))
              (merge-sort-rec vector p q)
              (merge-sort-rec vector (+ q 1) r)
              (merge vector p q r))))
      (merge-sort-rec vector 0 (- (vector-length vector) 1))
      vector)
    ))

(define my-list (list 8 7 5 56 798 2 4 657 51))
(bubble-sort my-list <)
(display my-list)

(newline)

(define my-vector (vector 8 7 5 56 798 2 4 657 51))
(insertion-sort my-vector <)
(display my-vector)

(newline)

(define my-newest-vector (vector 8 7 5 56 798 2 4 657 51))
(display my-newest-vector)
(newline)
(display (selection-sort my-newest-vector <))

; WPO 9 H5 last part

