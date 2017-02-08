(define (close-enough a b)
  (< (- b a) 0.001))

(define (positive a) (> a 0))
(define (negative a) (< a 0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive test-value)
                (search f neg-point midpoint))

                ((negative test-value)
                 (search f midpoint pos-point))

                (else midpoint))))))
