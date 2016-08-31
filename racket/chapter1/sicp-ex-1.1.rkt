#lang planet neil/sicp

; ex 1.2
(/
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))))
 (* 3 (- 6 2) (- 2 7)))

; ex 1.3
(define (square-larger-2 a b c)
  (cond
    ((and (>= a c) (>= b c))
     (+ (* a a) (* b b)))
    ((and (>= b a) (>= c a))
     (+ (* b b) (* c c)))
    (else
     (+ (* a a) (* b b)))))

; 1.6
; inifinte loop because of applicative order evaluation!

; 1.7
(define (avg x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough-guess? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough-guess? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; ex 1.8
(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cbrt x)
  (cbrt-iter 1. x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (cbrt-iter (improve guess x) x)))

(cbrt 217)
