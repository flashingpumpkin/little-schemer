#lang scheme

;; Exercises from The Little Schemer


; Is x an atom?
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

; Is l a list of atoms?
(define lat?
  (lambda (l)
    (cond ((null? l) #t)
          ((atom? (car l)) (lat? (cdr l)))
          (else #f))))

; Is a a member of lat?
(define member?
  (lambda (a lat)
    (cond 
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; Remove a from lat
(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons 
             (car lat)
             (rember a (cdr lat)))))))

; Return the cars of a list of lists
(define firsts 
  (lambda (l)
    (cond 
      ((null? l) (quote ()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

; Insert new after old, once
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) (cons old 
                                         (cons new 
                                               (cdr lat))))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))))

; Insert new before old, once
(define insertL 
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))

; Substitute old with new
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond 
              ((eq? old (car lat)) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

; Replace either o1 or o2 with new
(define subst2
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) (quote ()))
      (else (cond
              ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

; Remove every a from lat
(define multirember
  (lambda (a lat)
    (cond 
      ((null? lat ) (quote ()))
      (else (cond 
              ((eq? a (car lat)) (multirember a (cdr lat)))
              (else 
               (cons (car lat)
                     (multirember a (cdr lat)))))))))

; Insert new after each old
(define multiinsertR 
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      (else 
       (cond 
         ((eq? old (car lat))
          (cons old
                (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

; Insert new before each old
(define multiinsertL
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      (else 
       (cond
         ((eq? old (car lat))
          (cons new 
                (cons old (multiinsertL new old (cdr lat)))))
         (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

; Replace every occurance of old
(define multisubst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      (else 
       (cond 
         ((eq? old (car lat))
          (cons new (multisubst new old (cdr lat))))
         (else 
          (cons (car lat)
                (multisubst new old (cdr lat)))))))))


;; Numbers games

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+ 
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
       (sub1 (o- n (sub1 m)))))))

; Sum a tuple
(define addtup
  (lambda (tup)
    (cond 
      ((null? tup) 0)
      (else (+ (car tup)
               (addtup (cdr tup)))))))

; Multiplication
(define * 
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else 
       (+ n (* n (sub1 m)))))))

; Sum two tuples into one
(define tup+ 
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(define = 
  (lambda (n m)
    (cond 
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (= (sub1 n) (sub1 m))))))

(define =2
  (lambda (n m)
    (cond 
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define ^
  (lambda (n m)
    (cond 
      ((zero? m) 1)
      (else (* n (^ n (sub1 m)))))))

(define /
  (lambda (n m)
    (cond 
      ((< n m) 0)
      (else (add1 (/ (o- n m) m))))))

(define length
  (lambda (lat)
    (cond 
      ((null? lat) 0)
      (else 
       (add1 (length (cdr lat)))))))


(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond 
      ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) 
             (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond 
         ((number? (car lat)) (no-nums (cdr lat)))
         (else 
          (cons (car lat) (no-nums (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond 
      ((null? lat) (quote ()))
      (else
       (cond
         ((number? (car lat))
          (cons (car lat)
                (all-nums (cdr lat))))
         (else
          (all-nums (cdr lat))))))))
          
(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b))
       (= a b))
      ((or (number? a) (number? b))
       #f)
      (else
       (eq? a b)))))

(define occur
  (lambda (a lat)
    (cond 
      ((null? lat) 0)
      (else
       (cond
         ((eq? a (car lat))
          (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(define one?
  (lambda (a)
    (= a 1)))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick2 (sub1 n) (cdr lat)))))))

