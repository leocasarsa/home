;; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;; Exercise 2.59
(define (remove-element x set)
  (remove (lambda (y) (eq? y x)) set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (cons (car set1)
               (union-set (cdr set1)
                          (remove-element (car set1) set2))))))

;; Exercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (union-set (cdr set1)  set2)))
                 ((< x2 x1)
                  (cons x2
                        (union-set set1  (cdr set2)))))))))
