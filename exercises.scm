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