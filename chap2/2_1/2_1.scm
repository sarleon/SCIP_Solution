;2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (prefix-neg (cond ((and (> n 0) (> d 0))  1 )
                          ((and (< n 0) (< d 0))  1)
                          (else -1)
                    )))
    (cons (* (/ n g) prefix-neg) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equals-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)

(print-rat (mul-rat one-half one-half))

(print-rat (mul-rat one-half (make-rat -1  3)))
