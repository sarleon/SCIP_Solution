(define (sum term a next b)
  (if (> a b)
      0
      (+ ( term a)
         (sum term (next a) next b))))
(define (cube x) (* x x x))
(define (inc n ) ( + n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum cube  1 inc  10)
(define (integral f a b dx)
  (define (add-dx x) (define (add-dx x) (+ x dx))
                  (* (sum f (+ a (/ dx 2.0)) add-dx b)
                     dx)))

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a ) result))))
  (iter a 0))
(sum-i cube 1 inc 10)




;1.31
(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(product-i cube 1 inc 10)
(define (factorial i)
  (product-i (lambda (x) x) 1 inc i))

(factorial 10)



;1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if(> a b)
       result
       (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-2 term a next b )
  (accumulate + 0 term a next b))



;1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (filtered-iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (filtered-iter a null-value))


(define (f x y)
  ((lambda (a b)
     (+ (* x (* x x))
           (* y b)
           (* a b)))
     (+ 1 (* x y))
     (- 1 y)))



(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (* a a))
       (* y b)
       (* a b))))






;1.34

(define (square x) (* x x))

(define (f g)
  (g 2))

(f square)
(f f)
