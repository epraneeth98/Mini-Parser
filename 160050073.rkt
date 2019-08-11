#lang racket
(provide combine-cc
         combine-sc
         combine-cs
         combine-ss
         pred-p
         single-digit-p
         single-alphabet-p
         seq
         alt
         epsilon-p
         zero-or-more
         one-or-more
         whitespace-p number-p
         identifier-p
         variable-p
         term-p
         expression-p
         assignment-p)
         
(define-struct gnode (sym list) #:transparent)
(define-struct ident (str) #:transparent)
(define-struct num (val) #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (combine-cc char1 char2)
(list->string (list char1 char2)))
(define (combine-sc str char)
(list->string (append (string->list str)
(list char))))
(define (combine-cs char str)
(list->string (cons char (string->list str))))
(define (combine-ss str1 str2)
(list->string (append (string->list str1)
(string->list str2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (is-alphabet x)
  (if  (and (> (char->integer x) 96)
            (< (char->integer x) 123)) #t #f))

(define (is-number x)
  (if  (and (> (char->integer x) 47)
            (< (char->integer x) 58)) #t #f))

(define (give-num s)
  (if (char? s) (char->integer s)
      (give-num (string-ref s 0))))

(define (first-occurence str char1)
  (define (helper str char1 numb)
    (cond [(equal? "" str) 'not-found]
          [(equal? (string-ref str 0) char1) numb]
          [#t (helper (substring str 1) char1 (+ 1 numb))]))
  (helper str char1 0))

(define (last-occurence str char1)
  (if (equal? (first-occurence str char1) 'not-found)
      'not-found
  (- (- (string-length str) (first-occurence (inverse str) char1)) 1)))

(define (inverse str)
  (define (helper str list)
  (cond [(null? list) str]
        [#t (helper (combine-sc str (car list)) (cdr list))]))
  (helper "" (reverse (string->list str))))

(define (remove-ws str)
  (define (helper str1 str2)
    (cond [(equal? str2 "") str1]
      [(equal? (string-ref str2 0) #\ ) (helper str1 (substring str2 1))]
        [(not (equal? (string-ref str2 0) #\ )) (helper (combine-sc str1 (string-ref str2 0)) (substring str2 1))]))
  (helper "" str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pred-p p) (lambda (str) (if (p (string-ref str 0))
                                     (cons (string-ref str 0) (substring str 1))
                                     'fail)))

(define single-digit-p (lambda (str) (if  (and (< (char->integer (string-ref str 0)) 58)
                                               (> (char->integer (string-ref str 0)) 47))
                                         (cons (string-ref str 0) (substring str 1))
                                         'fail)))

(define single-alphabet-p (lambda (str) (if  (or (and (< (char->integer (string-ref str 0)) 123)
                                               (> (char->integer (string-ref str 0)) 96)) 
                                              (and (< (char->integer (string-ref str 0)) 91)
                                               (> (char->integer (string-ref str 0)) 64)))
                                         (cons (string-ref str 0) (substring str 1))
                                         'fail)))

(define epsilon-p (lambda (str) (cons "" str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (seq p1 p2 f) (lambda (str)
                        (if (equal? (p1 str) 'fail) 'fail
                            (cond [(equal? (p2 (cdr (p1 str))) 'fail) 'fail]
                                  [#t (cons (f (car (p1 str)) (car (p2 (cdr (p1 str)))))
                                         (cdr (p2 (cdr (p1 str)))))]))))
                   

(define (alt p1 p2) (lambda (str) (if (equal? (p1 str) 'fail) (p2 str) (p1 str))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (whitespace-p str)
  (define (helper n str)
    (if (equal? (string-ref str 0) #\ ) (helper (+ n 1) (substring str 1))
        (cons (make-string n #\ ) str)))
  (helper 0 str))

(define (zero-or-more p f)
  (lambda (str) (define (helper p str1 str2)
                  (cond [(= 0 (string-length str2)) (cons str1 "")]
                    [(equal? 'fail (p str2)) (cons str1 str2)]
                      [#t (helper p (string-append str1 (string (car (p str2)))) (substring str2 1))]))
    (helper p "" str)))

(define (one-or-more p f)
  (lambda (str) (define (helper p str1 str2)
                  (cond [(= 0 (string-length str2)) (cons str1 "")]
                    [(equal? 'fail (p str2)) (cons str1 str2)]
                      [#t (helper p (string-append str1 (string (car (p str2)))) (substring str2 1))]))
    (if (equal?  0 (string-length (car (helper p "" str)))) 'fail
        (helper p "" str))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (check-dot list1)
  (cond [(equal? '() list1) #f]
        [(equal? (car list1) #\.) #t]
        [#t (check-dot (cdr list1))]))

(define (pow a b)
  (if (= b 0) 1
      (* a (pow a (- b 1)))))

(define (starta list1)
  (define (helper lista list1)
    (if (equal? (car list1) #\.) (list lista (cdr list1))
        (helper (append lista (list (car list1))) (cdr list1))))
  (helper '() list1))

(define (convert list2)
    (define (helper list1)
    (if (equal? '() list1) 0
        (+ (- (char->integer (car list1)) 48) (* 10 (helper (cdr list1))))))
  (helper (reverse list2)))

(define (to-decimal alist)
  (if (not (check-dot alist)) (convert alist)
      (* 1.0 (+ (convert (car (starta alist)))
         (/ (convert (car (cdr (starta alist)))) (pow 10 (length (car (cdr (starta alist))))))))))

(define (str->num str)
  (to-decimal (string->list str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define number-p
  (lambda (str)
    (define (helper str1 str2)
      (cond [(equal? str2 "") (cons (num (str->num str1)) "")]
        [(and (> (char->integer (string-ref str2 0)) 47)
               (< (char->integer (string-ref str2 0)) 58))
          (helper (combine-sc str1 (string-ref str2 0)) (substring str2 1))]
            [#t (cons (num (str->num str1)) str2)]))
    (if (= (num-val (car (helper "" (cdr (whitespace-p str))))) 0) 'fail
        (helper "" (cdr (whitespace-p str))))))

(define starting
  (lambda (str)
    (if (and (> (char->integer (string-ref (cdr (whitespace-p str)) 0)) 96)
               (< (char->integer (string-ref (cdr (whitespace-p str)) 0)) 123))
        (cons (string-ref (cdr (whitespace-p str)) 0) (substring (cdr (whitespace-p str)) 1))
        'sorry)))

(define remaining
  (lambda (str)
    (define (helper str1 str2)
    (cond [(equal? str2 "") (cons str1 "")]
          [(or (is-alphabet (string-ref str2 0))
               (is-number (string-ref str2 0))) (helper (combine-sc str1 (string-ref str2 0)) (substring str2 1))]
          [#t (cons str1 str2)]))
    (helper "" str)))

(define identifier-p
  (lambda (str) (if (equal? (starting str) 'sorry)
                    'fail
                    (cons (ident (combine-cs (car (starting str)) (car (remaining (cdr (starting str))))))
                          (cdr (remaining (cdr (starting str))))))))
(define (variable-p str)
  (if (equal? "" (cdr (identifier-p str))) (identifier-p str)
      (cons (gnode 'ARRAY (list (expression-p (substring (cdr (identifier-p str)) 1
                                                         (- (string-length (cdr (identifier-p str))) 1))))) "")))

(define (expression-p str) (cond [(equal? "" str) (cons "" str)]
                                 [(equal? #\) (string-ref str 0)) (expression-p (substring str 1))]
                                 [(equal? #\+ (string-ref str 0)) (expression-p (substring str 1))]
                                 [#t (cons (gnode 'PLUS (list (car (term-p str)) (expression-p (cdr (term-p str))))) "")]))

(define (term-p str)
  (cond [(not (equal? 'fail (number-p str))) (number-p str)]
        [(not (equal? 'fail (identifier-p str))) (identifier-p str)]
        [#t (expression-p (substring str 1))]))

(define (assignment-p str) (gnode 'ASSIGN (list (variable-p (substring str 0 (first-occurence str #\=)))
                                              (expression-p (substring str (+ 1 (first-occurence str #\=)))))))