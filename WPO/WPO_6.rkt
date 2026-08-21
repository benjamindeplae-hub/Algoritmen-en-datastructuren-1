#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d stack linked) stack:))

; H4 start

; Oef 1
(define (post-fix-eval lst)
  (define (iter list stack res)
    (cond ((null? list) res)
          ((number? (car list))
           (stack:push! stack (car list))
           (iter (cdr list) stack res))
          (else
             
           (let loop ()
             (if (not (stack:empty? stack))
                 (let ((value (stack:pop! stack)))
                   (set! res ((car list) res value))
                   (loop))
                 (iter (cdr list) stack res))))))
  
  (iter lst (stack:new) 0))

(display (post-fix-eval (list 5 6 +))) ; 11
(newline)
(display (post-fix-eval (list 5 6 + 7 -))) ; 4
(newline)
(display (post-fix-eval (list 5 6 7 + 4 5 6 4 -))) ; - 1
(newline)
(newline)

; Oef 2
(define (extract-name s)
  (if (eq? (string-ref s 1) #\/)
      (substring s 2 (- (string-length s) 1))
      (substring s 1 (- (string-length s) 1))))

(define (valid? lst)
  (define (iter list stack)
    (cond ((and (null? list) (stack:empty? stack)) #t)
          ((null? list) #f)
          (else
           (let ((list-elem (symbol->string (car list))))
             (cond
               ((= (string-length list-elem) 1)
                  (iter (cdr list) stack))
               
               ((eq? (string-ref list-elem 1) #\/)
                  (let ((stack-elem (stack:pop! stack)))
                    (if (string=? (extract-name list-elem) (extract-name stack-elem))
                      (iter (cdr list) stack)
                      #f)))
          
               ((eq? (string-ref list-elem 0) #\<)
                  (stack:push! stack list-elem)
                  (iter (cdr list) stack))
          
               (else (iter (cdr list) stack)))))))
  (iter lst (stack:new)))

(display (valid? '(<html> <head> This is the head </head> <body> And this is the body </body> </html>))) ; #t
(newline)
(display (valid? '(<html> <head> This is the head </head> <body> And this is the body </body></html>))) ; #f wegens </body></html>
(newline)
(display (valid? '(<html> <head> This is the <head </head> <body> And this is the body </body> </html>))) ; #f wegens <head
(newline)
(display (valid? '(<html> <head> This is the head </head> <body> And this is the body </html>))) ; #f wegens geen close </body>
(newline)
(display (valid? '(<html> <head> This is < the head </head> <body> And this is the body </body> </html>))) ; #t test voor < enkel karakter
(newline)
(newline)
