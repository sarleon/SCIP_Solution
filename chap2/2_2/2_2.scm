(cons 1 (cons 2 (cons 3 (cons 4 '()))))

(list 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (cat items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))


(define (iter-r remained-items result)
  (if (null? remained-items)
      result
      (iter-r (cdr remained-items)
                          (cons (car remained-items) result))))

(define (reverse items)
  (iter-r items '()))

(define l (list 1 2 5 9))

(last-pair l)
(reverse l)
