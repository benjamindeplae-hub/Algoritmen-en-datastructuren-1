#lang r7rs
(import (scheme base)
        (scheme write)
        (racket/trace))


; H2 Exercise


; Oef 1
(char->integer #\0)

(char->integer #\9)

(char->integer #\a)

(char->integer #\z)

(char->integer #\A)

(char->integer #\Z)

; Oef 2
(define (string->number str)
  (let loop ((0-start (char->integer #\0)))
    (let loop ((i (- (string-length str) 1))
               (res 0))
      (if (< i 0)
          res
          (loop (- i 1)
                (+ res (* (- (char->integer (string-ref str i)) 0-start)
                          (expt 10 (- (- (string-length str) 1) i)))))))))
(display (string->number "123"))
; performance O(n)

; Oef 3
; 1 prefix
; 2 suffix
; 3 ja want de prefix is niet de lege string noch de string zelf

; Oef 4
; proper prefixes: H, He, Hel, Hell
; proper sufffixes: o, lo, llo, ello

; Oef 5
; match (string string -> number U {#f})

(newline)
; Oef 6
(define (match-bf t p)
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

(define (match-bf-all t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (let loop
    ((i-t 0)      ;; Current index in the text
     (i-p 0)      ;; Current index in the pattern
     (matches '())) ;; Accumulated list of matches
    (cond
      ;; Pattern fully matched, add starting index and reset pattern index.
      ((> i-p (- n-p 1))
       (loop (+ i-t 1) 0 (cons i-t matches)))

      ;; Text exhausted: return the list of matches in the correct order.
      ((> i-t (- n-t n-p))
       (reverse matches))

      ;; Characters match: continue matching the pattern.
      ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
       (loop i-t (+ i-p 1) matches))

      ;; Characters do not match: move to the next position in the text.
      (else
       (loop (+ i-t 1) 0 matches)))))

(display (match-bf-all "babaya" "ba"))

; Oef 7
; ...


; WPO 4 start


; Oef 8
; papa
; patat

(newline)
(newline)
; Oef 9
(define (match-kmp t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (define sigma (compute-failure-function p))
  (let loop
    ((i-t 0)
     (i-p 0))

    ; Oef 9 start
    (display t)
    (newline)
    (display (make-string i-t #\space))
    (display p)
    (newline)
    (newline)
    ; Oef 9 end
    
    (cond
      ((> i-p (- n-p 1))
       i-t)
      ((> i-t (- n-t n-p))
       #f)
      ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
       (loop i-t (+ i-p 1)))
      (else
       (loop (+ i-t (- i-p (sigma i-p))) (if (> i-p 0)
                                             (sigma i-p)
                                             0))))))

(define (compute-failure-function p)
  (define n-p (string-length p))
  (define sigma-table (make-vector n-p 0))
  (let loop
    ((i-p 2)
     (k 0))
    (when (< i-p n-p)
      (cond
        ((eq? (string-ref p k)
              (string-ref p (- i-p 1)))
         (vector-set! sigma-table i-p (+ k 1))
         (loop (+ i-p 1) (+ k 1)))
        ((> k 0)
         (loop i-p (vector-ref sigma-table k)))
        (else ; k=0
         (vector-set! sigma-table i-p 0)
         (loop (+ i-p 1) k)))))
  (vector-set! sigma-table 0 -1)
  (lambda (q)
    (vector-ref sigma-table q)))

(display (match-kmp "ABC ABCDAB ABCDABCDABDE" "ABCDABD"))

; Oef 10
; (string -> (number -> number))

; Oef 11
;  a b r a c a d a b r a
; -1 0 0 0 1 0 1 0 1 2 3

; Oef 12
;  h a a h i i h a a h a a h i i
; -1 0 0 0 1 0 0 1 2 3 4 2 3 4 5

; KMP shift adhv prefix en suffix
; QuickSearch shift adhv volgende letter matchen op het voorkomen van die letter in het patroon en allignen
; vb: text: My stepsister patroon: stepping
; My stepsister
; stepping
; mismatch => volgende letter "i" zit in patroon, dus shiften zodat stepping gealligned is op die "i"
; My stepsister
;    stepping

; Oef 13
; vb: text: aaaaaa patroon: aba
; a a a a a a
; a b a

; a a a a a a
;   a b a

; a a a a a a
;     a b a

; a a a a a a
;       a b a

; laatste karakter van patroon is telkens gematched met de volgende letter in de text na een mismatch
; plus het patroon moet constant helemaal matchen tot het voorlaatste karakter zodat de matching zolang mogelijk over het patroon blijft gaan, en dan pas 1'tje kan opschuiven
; voorlaatste karakter omdat er nogsteeds mismatch moet zijn, plus laatste karakter moet wel matchen om maar + 1 op te schuiven

(newline)
(newline)
; Oef 14
(define (match-qs t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (define shift (compute-shift-function p))
  ; Oef 14
  (define mismatch-shifts '())
  ; Oef 14 end
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
       (let ((c-t (string-ref t (modulo (+ i-t n-p) n-t))))
         ; Oef 14
         (set! mismatch-shifts (cons (+ i-t (shift c-t)) mismatch-shifts))    
         ; Oef 14 end
         
         (loop (+ i-t (shift c-t)) 0)))))
    ; Oef 14
    (display (reverse mismatch-shifts))
    ; Oef 14 end
  )

(define (compute-shift-function p)
  (define n-p (string-length p))
  (define min-ascii (char->integer (string-ref p 0)))
  (define max-ascii min-ascii)
  (define (create-table index)
    (if (< index n-p)
        (begin
          (set! min-ascii (min min-ascii (char->integer (string-ref p index))))
          (set! max-ascii (max max-ascii (char->integer (string-ref p index))))
          (create-table (+ index 1)))
        (make-vector (- max-ascii min-ascii -1) (+ n-p 1))))
  (define (fill-table index)
    (if (< index n-p)
        (let ((ascii (char->integer (string-ref p index))))
          (vector-set! shift-table (- ascii min-ascii) (- n-p index))
          (fill-table (+ index 1)))))
  (define shift-table (create-table 0))
  (fill-table 0)
  (lambda (c)
    (let ((ascii (char->integer c)))
      (if (>= max-ascii ascii min-ascii)
          (vector-ref shift-table (- ascii min-ascii))
          (+ n-p 1)))))

(define text1 "GGCAGCACGATCGCATGTCCCACGTGAACCATTGGTAAACCCTGTGGCCTGTGAGCGACAAAAGCTTTAATGGGAAATTCGCGCCCATAACTTGGTCCGAATACGGGTCCTAGCAACGTTCGTCTGAGTTTGATCTATATAATACGGGCGGTATGTCTGCTTTGATCAACCTCCAATAGCTCGTATGATAGTGCACCCGCTGGTGATCACTCAATGATCTGGGCTCCCCGTTGCAACTACGGGGATTTTTCGAGACCGACCTGCGTTCGGCATTGTGGGCACAGTGAAGTATTAGCAAACGTTAAGTCCCGAACTAGATGTGACCTAACGGTAAGAGAATTTCATAATACGTCCTGCCGCACGCGCAAGGTACATTTGGAAGTATTGAATGGACTCTGATCAACCTTCACACCGATCTAGAATCGAATGCGTAGATCAGCCAGGTGCAAACCAAAAATTCTAGGTTACTAGAAGTTTTGCGACGTTCTAAGTGTTGGACGAAATGATTCGCGACCCAGGATGAGGTCGCCCTAAAAAATAGATTTCTGCAACTCTCCTCGTGAGCAGTCTGGTGTATCGAAAGTACAGGACTAGCCTTCCTAGCAACCGCGGGCTGGGAGTCTGAGACATCACTCAAGATATATGCTCGGTAACGTATGCTCTAGCCATCTAACTATTCCCTATGTCTTATAGGGGCCTACGTTATCTGCCTGTCGAACCATAGGATTCGCGTCAGCGCGCAGGCTTGGATCGAGATGAAATCTCCGGAGCCTAAGACCACGAGCGTCTGGCGTCTTGGCTAATCCCCCTACATGTTGTTATAAACAATCAGTGGAAACTCAGTGCTAGAGGGTGGAGTGACCTTAAATCAAGGACGATATTAATCGGAAGGAGTATTCAACGCAATGAAGTCGCAGGGTTGACGTGGGAATGGTGCTTCTGTCCAAACAGGTAAGGGTATGAGGCCGCAACCGTCCCCCAAGCGTACAGGGTGCACTT")
(define patroon1 "GCAACCGTCCCCCAAGCGTACA")

(define text2 "Once upon a time, in a quaint village nestled between rolling hills and lush forests, there was a small community of artisans and farmers. The villagers were known for their craft and the quality of their produce. Every morning, the marketplace bustled with activity as people exchanged goods, shared stories, and formed bonds over the freshest bread, the ripest fruits, and the most intricate handcrafted items. Among them was a young blacksmith named Eric, whose reputation for creating the finest tools had spread far and wide. Eric was not only skilled in his craft but also known for his kindness and willingness to help anyone in need. One summer, as the village prepared for the annual harvest festival, Eric found himself particularly busy, fulfilling orders and repairing tools to ensure everything was ready for the celebrations. The festival was a time for joy, gratitude, and a showcase of the village's talents, drawing visitors from neighboring towns and far-off places.")
(define patroon2 "drawing visitors")

(match-qs text1 patroon1)
(match-qs text2 patroon2)

