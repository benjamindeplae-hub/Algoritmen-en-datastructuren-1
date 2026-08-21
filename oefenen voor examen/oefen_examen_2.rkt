#lang r7rs
(import (scheme base)
        (scheme write))

;; BST ADT
(define-record-type tree
  (bt:tree v l r)
  tree?
  (v bt:value bt:value!)
  (l bt:left bt:left!)
  (r bt:right bt:right!))

(define bt:null-tree '())

(define (bt:null-tree? node) (eq? node bt:null-tree))
;; BST ADT


(define-record-type rope
  (new tree)
  rope?
  (tree tree tree!))

;; Rope length (Oefening 3a)
(define (rope-length rope)
  (let traverse ((tree (tree rope)))
    (cond ((bt:null-tree? tree) 0)
          ((and (bt:null-tree? (bt:left tree))
                (bt:null-tree? (bt:right tree)))
           (car (bt:value tree)))
          (else (+ (traverse (bt:left tree))
                   (traverse (bt:right tree)))))))

;; Rope reference (Oefening 3b)
(define (rope-ref rope index)
  (let traverse ((tree (tree rope))
                 (prev 0))
    (cond ((and (bt:null-tree? (bt:left tree))
                (bt:null-tree? (bt:right tree)))
           (let ((string (cdr (bt:value tree)))
                 (i (- index prev)))
             (if (<= (string-length string) i )
                 (error "out of bounce")
                 (string-ref string (- index prev)))))
          ((< (+ (bt:value tree) prev) index) (traverse (bt:right tree) (+ (bt:value tree) prev)))
          ((> (+ (bt:value tree) prev) index) (traverse (bt:left tree) prev)))))

;; Delete if (Oefening 3c)
(define (delete-if! rope index proc)
  (define (decrement-path path)
    (if (not (null? path))
        (let ((tree (car path)))
          (if (pair? (bt:value tree))
              (bt:value! tree (set-car! (bt:value tree) (- (car (bt:value tree)) 1)))
              (bt:value! tree (- (bt:value tree) 1)))
          (decrement-path (cdr path)))))
  
  (let traverse ((tree (tree rope))
                 (prev 0)
                 (path '()))
    (cond ((and (bt:null-tree? (bt:left tree))
                (bt:null-tree? (bt:right tree)))
           
           (let* ((string (cdr (bt:value tree)))
                  (length (string-length string))
                  (i (- index prev)))
             (if (proc (string-ref string i))
                 (begin
                   (decrement-path path)
                   (bt:value! tree (cons (- length 1)
                                         (string-append
                                          (substring string 0 i)
                                          (substring string (+ i 1) length))))))))
          
          ((< (+ (bt:value tree) prev) index) (traverse (bt:right tree) (+ (bt:value tree) prev) path))
          ((> (+ (bt:value tree) prev) index) (traverse (bt:left tree) prev (cons tree path) )))))


(define testrope
  (new (bt:tree 22
                (bt:tree 9
                         (bt:tree 6
                                  (bt:tree (cons 6 "Hello_") bt:null-tree bt:null-tree)
                                  (bt:tree (cons 3 "my_") bt:null-tree bt:null-tree))
                         (bt:tree 6
                                  (bt:tree 2
                                           (bt:tree (cons 2 "na") bt:null-tree bt:null-tree)
                                           (bt:tree (cons 4 "me_i") bt:null-tree bt:null-tree))
                                  (bt:tree 1
                                           (bt:tree (cons 1 "s") bt:null-tree bt:null-tree)
                                           (bt:tree (cons 6 "_Simon") bt:null-tree bt:null-tree))))
                (bt:tree 9
                         (bt:tree (cons 9 "._Good_lu") bt:null-tree bt:null-tree)
                         (bt:tree (cons 3 "ck!") bt:null-tree bt:null-tree)))))
