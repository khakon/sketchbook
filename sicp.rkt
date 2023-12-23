#lang scheme

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-left1 op initial sequence)
  (if (null? sequence)
      initial
       (fold-left1 op (op initial (car sequence)) (cdr sequence))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (permutations s)
  (if (null? s)
      (list '())
      (map (lambda (x)
             (map (lambda (p) (cons x p))
                  (permutations (remove x s))))
           s)))

;(permutations (list 1 2 3))

(define (even-fibs n)
  (accumulate cons
              (list '())
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

;(even-fibs 10)

(accumulate / 1 (list 1 2 3))

(fold-left / 1 (list 1 2 3))

(fold-left1 / 1 (list 1 2 3))

(newline)