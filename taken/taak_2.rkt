#lang r7rs

(define-library (funky string tree)
  (export dodona-tree funky-string-tree max-string max-at-levels)
  (import (scheme base)
          (scheme write)
          (prefix (a-d tree binary-tree) bt:)
          (prefix (a-d dictionary ordered bst) dict:))

  (begin
    (define dodona-tree
      (bt:new 10
              (bt:new 5
                      (bt:new 2
                              bt:null-tree
                              bt:null-tree)
                      (bt:new 17
                              bt:null-tree
                              bt:null-tree))
              (bt:new 15
                      (bt:new 12
                              bt:null-tree
                              bt:null-tree)
                      (bt:new 1
                              bt:null-tree
                              bt:null-tree))))

    (define funky-string-tree
      (bt:new "apple"
              (bt:new "banana"
                      (bt:new "date"
                              (bt:new "kiwi" bt:null-tree bt:null-tree)
                              (bt:new "lime" bt:null-tree bt:null-tree))
                      (bt:new "elder"
                              (bt:new "mango" bt:null-tree bt:null-tree)
                              (bt:new "pear"  bt:null-tree bt:null-tree)))
              (bt:new "cherry"
                      (bt:new "fig"
                              (bt:new "plum"  bt:null-tree bt:null-tree)
                              (bt:new "peach" bt:null-tree bt:null-tree))
                      (bt:new "grape"
                              (bt:new "berry" bt:null-tree bt:null-tree)
                              (bt:new "melon" bt:null-tree bt:null-tree)))))

    (define (max-string str1 str2)
      (if (string>? str1 str2)
          str1
          str2))

    (define (max-at-levels tree max)
      (define result (dict:new = <))

      (define (traverse node level)
        (if (not (bt:null-tree? node))
            (begin
              (dict:insert! result level
                            (if (dict:find result level)
                                (max (dict:find result level) (bt:value node))
                                (bt:value node)))
              (traverse (bt:left node) (+ level 1))
              (traverse (bt:right node) (+ level 1)))))
      
      (traverse tree 1)
      result)))

(display (max-at-levels dodona-tree max))
(display (max-at-levels funky-string-tree max-string))