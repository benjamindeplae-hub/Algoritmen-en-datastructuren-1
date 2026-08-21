#lang r7rs

(define-library (sorted-list)
  (export new from-scheme-list sorted-list? empty? full? length
          find! delete! add! peek)
  (import (except (scheme base) length))
  (begin
    (define default-size 20)

    (define-record-type sorted-list
      (make-sorted-list s v l e)
      sorted-list?
      (s size size!)
      (v storage storage!)
      (l lesser)
      (e equality))
 
    (define (make len <<? ==?)
      (make-sorted-list 0 -1 (make-vector (max default-size len)) <<? ==?))
 
    (define (new <<? ==?)
      (make 0 <<? ==?))
 
    (define (from-scheme-list slst <<? ==?)
      (let loop
        ((lst slst)
         (idx 0))
        (if (null? lst)
            (make idx <<? ==?)
            (add! (loop (cdr lst) (+ idx 1)) (car lst)))))
  
    (define (storage-move-right vector i j)
      (define (iter idx)
        (vector-set! vector (+ idx 1) (vector-ref vector idx))
        (if (> idx i)
            (iter (- idx 1))))
      (iter j))
    (define (storage-move-left vector i j)
      (define (iter idx)
        (vector-set! vector (- idx 1) (vector-ref vector idx))
        (if (< idx j)
            (iter (+ idx 1))))
      (iter i))
 
    (define (length slst)
      (size slst))
 
    (define (empty? slst)
      (= (length slst) 0))
 
    (define (full? slst)
      (= (length slst)
         (vector-length (storage slst))))
 
    (define (find-sequential! slst key)
      (define ==? (equality slst))
      (define <<? (lesser slst))
      (define vect (storage slst))
      (define leng (size slst))
      (let sequential-search
        ((curr 0))
        (cond ((>= curr leng) #f)
              ((==? key (vector-ref vect curr)) curr)
              ((<<? (vector-ref vect curr) key)
               (sequential-search (+ curr 1)))
              (else #f))))
 
    (define (find! slst key)
      (define ==? (equality slst))
      (define <<? (lesser slst))
      (define vect (storage slst))
      (define leng (size slst))
      (let binary-search
        ((left 0)
         (right (- leng 1)))
        (if (<= left right)
            (let ((mid (quotient (+ left right 1) 2)))
              (cond
                ((==? (vector-ref vect mid) key) mid)
                ((<<? (vector-ref vect mid) key)
                 (binary-search (+ mid 1) right))
                (else (binary-search left (- mid 1)))))
            #f)))
 
    (define (delete! slst pos)
      (define vect (storage slst))
      (define last (size slst))
      (if (< (+ pos 1) last)
          (storage-move-left vect (+ pos 1) last)
          (error "index out of bounce" slst))
      (size! slst (- last 1))
      slst)
 
    (define (peek slst pos)
       (if (< (+ pos 1) (size slst))
          (vector-ref (storage slst) pos)
          (error "index out of bounce" slst)))

    (define (rescale-list plst)
      (define old-vector (storage plst))
      (define old-size (vector-length old-vector))
      (define new-size (* 2 old-size))
      (define new-vector (make-vector new-size))

      (storage! plst new-vector)
      (let rehash-elements ((i 0))
        (when (< i old-size)
          (let ((assoc (vector-ref old-vector i)))
            (vector-set! new-vector i assoc)
            (rehash-elements (+ i 1))))))
    
    (define (add! slst val)
      (define <<? (lesser slst))
      (define vect (storage slst))
      (define leng (size slst))
      (if (= leng (vector-length vect))
          (error "list full (add!)" slst))
      (let vector-iter
        ((idx leng))
        (cond 
          ((= idx 0)
           (vector-set! vect idx val))
          ((<<? val (vector-ref vect (- idx 1)))
           (vector-set! vect idx (vector-ref vect (- idx 1)))
           (vector-iter (- idx 1)))
          (else
           (vector-set! vect idx val))))
      (size! slst (+ leng 1))
      slst)))