#lang racket

(require "Solver.rkt")

(define (pos->symbol row col)
  (string->symbol
   (string-append
    "p" (number->string row) (number->string col))))

(define col-counts '(2 3 1 5 3 1 3 5))
(define row-counts '(5 3 3 2 5 0 4 1))

(define monsters '())
(define treasures '((2 . 5)))

(define (solve-board col-counts row-counts monsters treasures)
  
  (define cols (length col-counts))
  (define rows (length row-counts))

  (define monster-vars (map (λ (p) (pos->symbol (car p) (cdr p))) monsters))
  (define treasure-vars (map (λ (p) (pos->symbol (car p) (cdr p))) treasures))

  (define (monster? p)
    (and (member p monster-vars) #t))
  (define (treasure? p)
    (and (member p treasure-vars) #t))

  (define var-defs
    (for*/list ([i (in-range rows)]
                [j (in-range cols)])
      `(declare-const ,(pos->symbol i j) Bool)))

  (define (choose n lst)
    (if (zero? n)
        '(())
        (match lst
          ['() '()]
          [(cons x rst)
           (append (map (λ (tl) (cons x tl)) (choose (sub1 n) rst))
                   (choose n rst))])))

  (define (any-n n . lst)
    (let ([conds (map (λ (ps) `(and ,@ps)) (choose n lst))])
      (if (empty? conds)
          'false
          `(or ,@conds))))

  (define (exactly-n n . lst)
    (let ([conds (map (λ (ps)
                        `(and ,@ps
                              ,@(map (λ (p) `(not ,p))
                                     (filter (λ (p) (not (member p ps))) lst))))
                      (choose n lst))])
      (if (empty? conds)
          'false
          `(or ,@conds))))

  (define col-counts-correct
    (for/list ([j (in-naturals)]
               [n col-counts])
      (apply exactly-n (cons n (for/list ([i (in-range rows)])
                                 (pos->symbol i j))))))
  (define row-counts-correct
    (for/list ([i (in-naturals)]
               [n row-counts])
      (apply exactly-n (cons n (for/list ([j (in-range cols)])
                                 (pos->symbol i j))))))

  ;; p1
  ;; p2 p3
  (define (not-deadend-corner p1 p2 p3)
    (cond
      [(monster? p2) `(and (not ,p2) ,(exactly-n 1 p1 p3))]
      [(treasure? p2) `(not ,p2)]
      [else `(=> (or ,p1 ,p3) ,p2)]))

  ;;    p1
  ;; p2 p3 p4
  (define (not-deadend-wall p1 p2 p3 p4)
    (cond
      [(monster? p3) `(and (not ,p3) ,(exactly-n 2 p1 p2 p4))]
      [(treasure? p3) `(not ,p3)]
      [else `(=> ,(any-n 2 p1 p2 p4) ,p3)]))

  ;;    p1
  ;; p2 p3 p4
  ;;    p5
  (define (not-deadend p1 p2 p3 p4 p5)
    (cond
      [(monster? p3) `(and (not ,p3) ,(exactly-n 3 p1 p2 p4 p5))]
      [(treasure? p3) `(not ,p3)]
      [else `(=> ,(any-n 3 p1 p2 p4 p5) ,p3)]))

  (define no-deadends
    (append
     ;; Middle blocks
     (for*/list ([i (in-range 1 (sub1 rows))]
                 [j (in-range 1 (sub1 cols))])
       (not-deadend (pos->symbol (sub1 i) j)
                    (pos->symbol i (sub1 j))
                    (pos->symbol i j)
                    (pos->symbol i (add1 j))
                    (pos->symbol (add1 i) j)))

     ;; Wall blocks
     (for*/list ([i (in-range 1 (sub1 rows))])
       (not-deadend-wall (pos->symbol i 1)
                         (pos->symbol (sub1 i) 0)
                         (pos->symbol i 0)
                         (pos->symbol (add1 i) 0)))
     (for*/list ([i (in-range 1 (sub1 rows))])
       (not-deadend-wall (pos->symbol i (- cols 2))
                         (pos->symbol (sub1 i) (sub1 cols))
                         (pos->symbol i (sub1 cols))
                         (pos->symbol (add1 i) (sub1 cols))))
     (for*/list ([j (in-range 1 (sub1 cols))])
       (not-deadend-wall (pos->symbol 1 j)
                         (pos->symbol 0 (sub1 j))
                         (pos->symbol 0 j)
                         (pos->symbol 0 (add1 j))))
     (for*/list ([j (in-range 1 (sub1 cols))])
       (not-deadend-wall (pos->symbol (- rows 2) j)
                         (pos->symbol (sub1 rows) (sub1 j))
                         (pos->symbol (sub1 rows) j)
                         (pos->symbol (sub1 rows) (add1 j))))

     ;; Corner blocks
     (list
      (not-deadend-corner
       (pos->symbol 0 1)
       (pos->symbol 0 0)
       (pos->symbol 1 0))
      (not-deadend-corner
       (pos->symbol (sub1 rows) 1)
       (pos->symbol (sub1 rows) 0)
       (pos->symbol (- rows 2) 0))
      (not-deadend-corner
       (pos->symbol 0 (- cols 2))
       (pos->symbol 0 (sub1 cols))
       (pos->symbol 1 (sub1 cols)))
      (not-deadend-corner
       (pos->symbol (sub1 rows) (- cols 2))
       (pos->symbol (sub1 rows) (sub1 cols))
       (pos->symbol (- rows 2) (sub1 cols))))
     ))

  (define (intersects-treasure? row col)
    (ormap (λ (t)
             (<= (max (abs (- row (car t)))
                      (abs (- col (cdr t))))
                 2))
           treasures))

  (define no-double-halls
    (for*/list ([i (in-range (sub1 rows))]
                [j (in-range (sub1 cols))]
                #:when (not (and (intersects-treasure? i j)
                                 (intersects-treasure? i (add1 j))
                                 (intersects-treasure? (add1 i) j)
                                 (intersects-treasure? (add1 i) (add1 j)))))
      `(or ,(pos->symbol i j)
           ,(pos->symbol i (add1 j))
           ,(pos->symbol (add1 i) j)
           ,(pos->symbol (add1 i) (add1 j)))))

  (define (treasure-room row col)
    (let-values
        ([(wis wjs)
          (cond
            [(= 1 row col) (values '(2) '(2))] ; top left
            [(and (= 1 row) (= (- cols 2) col)) (values '(2) '(-2))] ; top right
            [(and (= (- rows 2) row) (= 1 col)) (values '(-2) '(2))] ; bottom left
            [(and (= (- rows 2) row) (= (- cols 2) col)) (values '(-2) '(-2))] ; bottom right
            [(= 1 row) (values '(2) '(2 -2))] ; top
            [(= (- rows 2) row) (values '(-2) '(2 -2))] ; bottom
            [(= 1 col) (values '(2 -2) '(2))] ; left
            [(= (- cols 2) col) (values '(2 -2) '(-2))] ; right
            [else (values '(-2 2) '(-2 2))])])
      `(and ,@(for*/list ([di '(-1 0 1)]
                          [dj '(-1 0 1)])
                `(not ,(pos->symbol (+ row di) (+ col dj))))
            ,(apply exactly-n
                    (cons (sub1 (* 3 (+ (length wis) (length wjs))))
                          (append
                           (for*/list ([di wis]
                                       [dj '(-1 0 1)])
                             (pos->symbol (+ row di) (+ col dj)))
                           (for*/list ([dj wjs]
                                       [di '(-1 0 1)])
                             (pos->symbol (+ row di) (+ col dj)))))))))

  (define treasure-rooms
    (map (λ (t)
           `(or ,@(for*/list ([di '(-1 0 1)]
                              #:when (<= 1 (+ (car t) di) (- rows 2))
                              [dj '(-1 0 1)]
                              #:when (<= 1 (+ (cdr t) dj) (- cols 2)))
                    (let ([room (treasure-room (+ (car t) di) (+ (cdr t) dj))])
                      (if (and (= (+ (abs di) (abs dj)) 2)
                               (<= 0 (+ (car t) (* -2 di)) (sub1 rows))
                               (<= 0 (+ (cdr t) (* -2 dj)) (sub1 cols)))
                          ;; add the no double hall check to the other corner
                          `(and ,room
                                (or ,(pos->symbol (+ (car t) (* -1 di)) (+ (cdr t) (* -1 dj)))
                                    ,(pos->symbol (+ (car t) (* -1 di)) (+ (cdr t) (* -2 dj)))
                                    ,(pos->symbol (+ (car t) (* -2 di)) (+ (cdr t) (* -1 dj)))
                                    ,(pos->symbol (+ (car t) (* -2 di)) (+ (cdr t) (* -2 dj)))))
                          room)))))
         treasures))

  (define all-asserts
    (map (λ (a) `(assert ,a))
         (append
          col-counts-correct
          row-counts-correct
          no-deadends
          no-double-halls
          treasure-rooms)))

  (let ([m (solve (append var-defs all-asserts))])
    (if (not m)
        #f
        (let ([tab (map (λ (b) (cons (second b) (fifth b))) m)])
          (for/list ([i (in-range rows)])
            (for/list ([j (in-range cols)])
              (cond
                [(member (cons i j) monsters) #\m]
                [(member (cons i j) treasures) #\t]
                [(equal? (dict-ref tab (pos->symbol i j)) 'true) #\X]
                [else #\space])))))))

(define (do-solve ccs rcs ms ts)
  (let ([b (solve-board ccs rcs ms ts)])
    (if (not b)
        (displayln "unsolvable")
        (displayln
         (string-join (map list->string b) "\n")))))






  

