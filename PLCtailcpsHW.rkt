
;Question 1: fib
(define fib-acc
  (lambda (n acc1 acc2)
    (cond
      ((zero? n) acc1)
      ((eq? n 1) acc2)
      (else (fib-acc (- n 1) acc2 (+ acc1 acc2))))))

(define fib
  (lambda (n)
    (fib-acc n 0 1)))


;Question 2: replaceallwith
(define replaceallwith-cps
  (lambda (x lis1 lis2 return)
    (cond
      ((or (null? lis1) (null? lis2)) (return lis1))
      ((eq? x (car lis1)) (replaceallwith-cps x (cdr lis1) (cdr lis2) (lambda (v) (return (cons (car lis2) v)))))
      (else (replaceallwith-cps x (cdr lis1) lis2 (lambda (v) (return (cons (car lis1) v))))))))

(define replaceallwith
  (lambda (x lis1 lis2)
    (replaceallwith-cps x lis1 lis2 (lambda (v) v))))


;Question 3: merge
(define merge-cps
  (lambda (lis1 lis2 return)
     (cond
     ((null? lis1) (return lis2))
     ((null? lis2) (return lis1))
     ((< (car lis1) (car lis2)) (merge-cps (cdr lis1) lis2 (lambda (v) (return (cons (car lis1) v)))))
     (else (merge-cps lis1 (cdr lis2) (lambda (v) (return (cons (car lis2) v))))))))

(define merge
  (lambda (lis1 lis2)
    (merge-cps lis1 lis2 (lambda (v) v))))


;Question 4: everyother
(define everyother-cps
  (lambda (lis return)
   (cond
     ((null? lis) (return lis))
     ((pair? (cdr lis)) (everyother-cps (cddr lis) (lambda (v) (return (cons (car lis) v)))))
     (else (return lis)))))

(define everyother
  (lambda (lis)
    (everyother-cps lis (lambda (v) v))))


;Question 5: everyother*
(define everyother*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return lis))
      ((and (list? (car lis)) (pair? (cdr lis)))  (everyother*-cps (car lis) (lambda (v1) (everyother*-cps (cddr lis) (lambda (v2) (return (cons v1 v2)))))))
      ((list? (car lis))                          (everyother*-cps (car lis) (lambda (v1) (return (cons v1 '())))))
      ((pair? (cdr lis))                          (everyother*-cps (cddr lis) (lambda (v1) (return (cons (car lis) v1)))))
      (else (return lis)))))

(define everyother*
  (lambda (lis)
    (everyother*-cps lis (lambda (v) v))))


;Question 6: everyotherback
(define split-cps
  (lambda (lis return)
    (if (null? lis)
        (return '() '())
        (split-cps (cdr lis) (lambda (v1 v2) (return (cons (car lis) v2) v1))))))

(define split
  (lambda (lis)
    (split-cps lis (lambda (v1 v2) (list v1 v2)))))


;(define everyotherback-cps
;  (lambda ( lis lis1 lis2 return)
;    (cond
;      ((null? lis) (return lis2))
;      ((null? (cdr lis)) (return lis1))
;      (else (everyotherback-cps (cddr lis) lis1 lis2 return)))))
;
;(define everyotherback
;  (lambda (lis)
;    (everyotherback-cps lis (car (split lis)) (cadr (split lis)) (lambda (v) v))))
(define everyotherback-cps
  (lambda (lis return1 return2)
    (cond
      ((null? lis) (return1 lis))
      ((null? (cdr lis)) (return2 lis))
      (else (everyotherback-cps (cddr lis) (lambda (v1) (return1 (cons (car lis) v1))) (lambda (v2) (return2 (cons (car lis) v2))))))))

(define everyotherback
  (lambda (lis)
    (everyotherback-cps lis (lambda (v) v) (lambda (v) v))))

    (everyotherback '(a b c d e f g))
    (everyotherback '(a b c d e f g h))



;Question 7: mergesort
(define mergesort-cps
  (lambda (lis return)
    (if (or (null? lis) (null? (cdr lis)))
        (return lis)
        (mergesort-cps (car (split lis)) (lambda (v1) (mergesort-cps (cadr (split lis)) (lambda (v2) (return (merge v1 v2)))))))))

(define mergesort
  (lambda (lis)
    (mergesort-cps lis (lambda (v) v))))



;Question 8: replaceallwith*
(define replaceallwith*-cps
  (lambda (x lis1 lis2 return)
    (cond
      ((or (null? lis1) (null? lis2)) (return lis1 lis2))
      ((list? (car lis1)) (replaceallwith*-cps x (car lis1) lis2 (lambda (v1 v2) (replaceallwith*-cps x (cdr lis1) v2 (lambda (v3 v4) (return (cons v1 v3) v4))))))
      ((eq? x (car lis1)) (replaceallwith*-cps x (cdr lis1) (cdr lis2) (lambda (v1 v2) (return (cons (car lis2) v1) v2))))
      (else (replaceallwith*-cps x (cdr lis1) lis2 (lambda (v1 v2) (return (cons (car lis1) v1) v2)))))))

(define replaceallwith*
  (lambda (x lis1 lis2)
    (replaceallwith*-cps x lis1 lis2 (lambda (v1 v2) (car (list v1 v2))))))



;Question 9: collapse-x
(define collapse-x-break
  (lambda (x lis xpassed break)
    (cond
      ((null? lis) lis)
      ((and (eq? x (car lis)) (not xpassed)) (cons x (call/cc (lambda (k)(collapse-x-break x (cdr lis) #t k)))))
      ((eq? x (car lis)) (break (collapse-x-break x (cdr lis) #t break)))
      ((eq? #t xpassed) (cons (car lis) (collapse-x-break x (cdr lis) #t break)))
      (else (cons (car lis) (collapse-x-break x (cdr lis) #f break))))))

(define collapse-x
  (lambda (x lis)
    (collapse-x-break x lis #f (lambda (v) v))))


;Question 10: xindex
(define xindex-break
  (lambda (x lis break count)
    (cond
      ((null? lis) lis)
      ((list? (car lis)) (cons (call/cc (lambda (k) (xindex-break x (car lis) k 1))) (xindex-break x (cdr lis) break (+ 1 count))))
      ((eq? (car lis) x) (break (cons count '())))
      (else (cons (car lis) (xindex-break x (cdr lis) break (+ 1 count)))))))

(define xindex
  (lambda (x lis)
    (call/cc (lambda (k) (xindex-break x lis k 1)))))
