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

(define (filter condition items)
  (display items) (newline)
  (if (null? items)
        '()
        (if (condition (car items))
            (cons (car items) (filter condition (cdr items)))
            (filter condition (cdr items)))))

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





(define (bigger-than-3 number) (> number 3))

(filter bigger-than-3 l)


(define (same-parity . z)
  (define (equals-first number )

    (= ((remainder (car z) 2))  (remainder  number 2) ))
  (if (null? z)
      '()
      (filter  equals-first   z)))

(same-parity 1 2 3 4 5 6 7)
