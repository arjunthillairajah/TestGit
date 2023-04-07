#lang racket
;(define multiply
;  (lambda (lis)
;    (cond
;      ((null? lis) 1)
;      ((zero? (car lis)) 0)
;      (else (* (car lis) (multiply (cdr lis)))))))

(define multiply-with-break
  (lambda (lis break)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) (break 0))
      (else (* (car lis) (multiply-with-break (cdr lis) break))))))

(define multiply
  (lambda (lis)
    (call/cc
     (lambda (k) (multiply-with-break lis k)))))

(define indexof-break
  (lambda (x lis break)
    (cond
      ((null? lis) (break -1))
      ((eq? x (car lis)) break 0)
      (else (+ 1 (indexof-break x (cdr lis) break))))))

(define indexof
  (lambda (x lis)
    (call/cc
     (lambda (k) (indexof-break x lis k)))))

(define append-cont 0)

(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        (call/cc (lambda (k) (begin (set! append-cont k)) l2))
        (cons (car l1) (myappend (cdr l1) l2)))))