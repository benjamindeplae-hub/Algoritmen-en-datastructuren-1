#lang r7rs

(define-library (brute-force)
  (export match)
  (import (scheme base)
          (scheme write))

  (begin
    (define (match t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
      (let loop
        ((i-t 0)
         (i-p 0))
        (cond
          ((> i-p (- n-p 1))
           i-t)
          ((> i-t (- n-t n-p))
           #f)
          ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
           (loop i-t (+ i-p 1)))
          (else
           (loop (+ i-t 1) 0)))))

    
    (define (match-reverse t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
      (let loop
        ((i-t (- n-t 1))
         (i-p (- n-p 1)))
        (cond
          ((< i-p 0)
           (- i-t (- n-p 1)))
          ((< i-t (- n-p 1))
           #f)
          ((eq? (string-ref t (- i-t (- n-p i-p 1))) (string-ref p i-p))
           (loop i-t (- i-p 1)))
          (else
           (loop (- i-t 1) (- n-p 1))))))))



(define text1 "GGCAGCACGATCGCATGTCCCACGTGAACCATTGGTAAACCCTGTGGCCTGTGAGCGACAAAAGCTTTAATGGGAAATTCGCGCCCATAACTTGGTCCGAATACGGGTCCTAGCAACGTTCGTCTGAGTTTGATCTATATAATACGGGCGGTATGTCTGCTTTGATCAACCTCCAATAGCTCGTATGATAGTGCACCCGCTGGTGATCACTCAATGATCTGGGCTCCCCGTTGCAACTACGGGGATTTTTCGAGACCGACCTGCGTTCGGCATTGTGGGCACAGTGAAGTATTAGCAAACGTTAAGTCCCGAACTAGATGTGACCTAACGGTAAGAGAATTTCATAATACGTCCTGCCGCACGCGCAAGGTACATTTGGAAGTATTGAATGGACTCTGATCAACCTTCACACCGATCTAGAATCGAATGCGTAGATCAGCCAGGTGCAAACCAAAAATTCTAGGTTACTAGAAGTTTTGCGACGTTCTAAGTGTTGGACGAAATGATTCGCGACCCAGGATGAGGTCGCCCTAAAAAATAGATTTCTGCAACTCTCCTCGTGAGCAGTCTGGTGTATCGAAAGTACAGGACTAGCCTTCCTAGCAACCGCGGGCTGGGAGTCTGAGACATCACTCAAGATATATGCTCGGTAACGTATGCTCTAGCCATCTAACTATTCCCTATGTCTTATAGGGGCCTACGTTATCTGCCTGTCGAACCATAGGATTCGCGTCAGCGCGCAGGCTTGGATCGAGATGAAATCTCCGGAGCCTAAGACCACGAGCGTCTGGCGTCTTGGCTAATCCCCCTACATGTTGTTATAAACAATCAGTGGAAACTCAGTGCTAGAGGGTGGAGTGACCTTAAATCAAGGACGATATTAATCGGAAGGAGTATTCAACGCAATGAAGTCGCAGGGTTGACGTGGGAATGGTGCTTCTGTCCAAACAGGTAAGGGTATGAGGCCGCAACCGTCCCCCAAGCGTACAGGGTGCACTT")
(define patroon1 "GCAACCGTCCCCCAAGCGTACA")

;                             s index 15, lengte patroon 16
;              drawing visitors
(define text2 "Once upon a time, in a quaint village nestled between rolling hills and lush forests, there was a small community of artisans and farmers. The villagers were known for their craft and the quality of their produce. Every morning, the marketplace bustled with activity as people exchanged goods, shared stories, and formed bonds over the freshest bread, the ripest fruits, and the most intricate handcrafted items. Among them was a young blacksmith named Eric, whose reputation for creating the finest tools had spread far and wide. Eric was not only skilled in his craft but also known for his kindness and willingness to help anyone in need. One summer, as the village prepared for the annual harvest festival, Eric found himself particularly busy, fulfilling orders and repairing tools to ensure everything was ready for the celebrations. The festival was a time for joy, gratitude, and a showcase of the village's talents, drawing visitors from neighboring towns and far-off places.")
(define patroon2 "drawing visitors")

(define text3 "ACGTACGTGACG")
(define patroon3 "ACGTACGTGACG")

(define text4 "The quick brown fox jumps over the lazy dog")
(define patroon4 "the lazy")

(define text5 "abababababab")
(define patroon5 "abab")

(define text6 "abcdefghijklmnopqrstuvwxyz")
(define patroon6 "123")

(define text7 "short")
(define patroon7 "longerpattern")

(define text8 "racecarlevelcivic")
(define patroon8 "level")

(define text9 "Look at the stars and the sky")
(define patroon9 "sky")



(display (match text1 patroon1))
(newline)
(display (match-reverse text1 patroon1))

(newline)
(newline)

(display (match text2 patroon2))
(newline)
(display (match-reverse text2 patroon2))

(newline)
(newline)

(display (match text3 patroon3))
(newline)
(display (match-reverse text3 patroon3))

(newline)
(newline)

(display (match text4 patroon4))
(newline)
(display (match-reverse text4 patroon4))

(newline)
(newline)

(display (match text5 patroon5))
(newline)
(display (match-reverse text5 patroon5))

(newline)
(newline)

(display (match text6 patroon6))
(newline)
(display (match-reverse text6 patroon6))

(newline)
(newline)

(display (match text7 patroon7))
(newline)
(display (match-reverse text7 patroon7))

(newline)
(newline)

(display (match text8 patroon8))
(newline)
(display (match-reverse text8 patroon8))

(newline)
(newline)

(display (match text9 patroon9))
(newline)
(display (match-reverse text9 patroon9))