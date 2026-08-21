
(define test-list (list "a" "c" "d" "e" "b"))

(define (insertion-sort lst <<?)
  (define (insert x sorted)
    (cond
      ((null? sorted) (list x))
      ((<<? x (car sorted)) (cons x sorted))
      (else (cons (car sorted) (insert x (cdr sorted))))))

  (define (iter unsorted sorted)
    (if (null? unsorted)
        sorted
        (iter (cdr unsorted) (insert (car unsorted) sorted))))
  (iter lst '()))



(insertion-sort test-list string<?)
