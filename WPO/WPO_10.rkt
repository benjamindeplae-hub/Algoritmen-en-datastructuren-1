#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-tree) bt:))

(define testboom (bt:new 1
                         (bt:new 2
                                 (bt:new 3
                                         bt:null-tree
                                         bt:null-tree)
                                 (bt:new 4
                                         bt:null-tree
                                         (bt:new 5
                                                 bt:null-tree
                                                 bt:null-tree)))
                         (bt:new 6 bt:null-tree bt:null-tree)))

(define (count-leafs tree)
  (if (bt:null-tree? tree)
      0
      (if (and (bt:null-tree? (bt:left tree)) (bt:null-tree? (bt:right tree)))
          1
          (+ (count-leafs (bt:left tree))
             (count-leafs (bt:right tree))))))

(define (depth tree)
  (if (bt:null-tree? tree)
      0
      (+ 1 (max (depth (bt:left tree))
                (depth (bt:right tree))))))

(define (height tree)
  (if (bt:null-tree? tree)
      0
      (- (depth tree) 1)))

(define (count-sub-trees tree)
  (define (count-sub-trees+1 tree)
    (if (bt:null-tree? tree)
        0
        (+ 1
           (count-sub-trees+1 (bt:left tree))
           (count-sub-trees+1 (bt:right tree)))))

  (- (count-sub-trees+1 tree) 1))

; root
     