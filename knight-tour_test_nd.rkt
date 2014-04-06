#lang racket
(define (initial l s)
  (if (< s 1) l (initial (append l '(0)) (- s 1))))

(define (change l posLi s)
  (if
    (equal? (length posLi) 0) l
    (and (for ([i (in-range 0 (* s s))])
      (vector-set! l (+ (* s (car (list-ref posLi i))) (cadr (list-ref posLi i))) (+ i 1)))
         (re-order (vector->list l) s))))

(define (re-order l s)
  (for ([i (in-range 0 (* s s))])
    (cond 
      [(equal? i 0) (display (~a "((" (list-ref l i) "\t"))]
      [(equal? (remainder i s) 0)
       (display (~a "(" (list-ref l i) "\t"))]
      [(equal? i (- (* s s) 1))
       (display (~a (list-ref l i) "))" "\n"))]
      [(equal? (remainder (+ i 1) s) 0)
       (display (~a (list-ref l i) ")" "\n"))]      
      [else (display (~a (list-ref l i) "\t"))])))

(define node 0)
(define (kt-depth posLi s m) ;position list, size, maximum
   (and (check (car (reverse posLi)) posLi s m) 
        (or (or (or (or (kt-depth (append posLi (list (list(+ (caar (reverse posLi)) 1) (+ (cadar (reverse posLi)) 2)))) s (- m 1))
        (kt-depth (append posLi (list (list (+ (caar (reverse posLi)) 1) (- (cadar (reverse posLi)) 2)))) s (- m 1)))
        (or (kt-depth (append posLi (list (list (- (caar (reverse posLi)) 1) (+ (cadar (reverse posLi)) 2)))) s (- m 1))
        (kt-depth (append posLi (list (list (- (caar (reverse posLi)) 1) (- (cadar (reverse posLi)) 2)))) s (- m 1))))
        (or (or (kt-depth (append posLi (list (list (+ (caar (reverse posLi)) 2) (+ (cadar (reverse posLi)) 1)))) s (- m 1))
        (kt-depth (append posLi (list (list (+ (caar (reverse posLi)) 2) (- (cadar (reverse posLi)) 1)))) s (- m 1)))
        (or (kt-depth (append posLi (list (list (- (caar (reverse posLi)) 2) (+ (cadar (reverse posLi)) 1)))) s (- m 1))
        (kt-depth (append posLi (list (list (- (caar (reverse posLi)) 2) (- (cadar (reverse posLi)) 1)))) s (- m 1)))))
        (= (length posLi) (* s s)))))
  
(define (check pos posLi s m)
  (cond 
    [(< m node) (and (display "Maximum trials reached!\n") #f)]
    [(member pos (cdr (reverse posLi))) (and (set! node (+ node 1)) #f)]
    [(> (car pos) (- s 1)) (and (set! node (+ node 1)) #f)]
    [(> (cadr pos) (- s 1)) (and (set! node (+ node 1))#f)]
    [(< (car pos) 0) (and (set! node (+ node 1)) #f)]
    [(< (cadr pos) 0) (and (set! node (+ node 1)) #f)]
    [(= (length posLi) (* s s)) 
     (change (list->vector (initial '() (* s s))) posLi s)
     (and (display (~a "Created node: " node))
          (set! node 0))]
    [else (and (+ node 1) #t)]))

(define (kt-depth-first point size maximum)
  (if (kt-depth (list point) size maximum)
      (display "\n")
      "No knight’s tour exists!\n" ))

(define (hquad pos s) (if (> (+ (+ (* (car pos) (- (- s 1) (car pos))) (* (cadr pos) (- (- s 1) (cadr pos))))) 0)
                          (+ (+ (* (car pos) (- (- s 1) (car pos))) (* (cadr pos) (- (- s 1) (cadr pos)))))
                          100))
(define (sort_point posLi s)
  (sort (list (list(+ (caar (reverse posLi)) 1) (+ (cadar (reverse posLi)) 2))
        (list (+ (caar (reverse posLi)) 1) (- (cadar (reverse posLi)) 2))
        (list (- (caar (reverse posLi)) 1) (+ (cadar (reverse posLi)) 2))
        (list (- (caar (reverse posLi)) 1) (- (cadar (reverse posLi)) 2))
        (list (+ (caar (reverse posLi)) 2) (+ (cadar (reverse posLi)) 1))
        (list (+ (caar (reverse posLi)) 2) (- (cadar (reverse posLi)) 1))
        (list (- (caar (reverse posLi)) 2) (+ (cadar (reverse posLi)) 1))
        (list (- (caar (reverse posLi)) 2) (- (cadar (reverse posLi)) 1)))
        (lambda (pos1 pos2) (< (hquad pos1 s) (hquad pos2 s)))))

(define (kt-best-first posLi s m) ;position list, size, maximum
  (and (check (car (reverse posLi)) posLi s m) 
        (or (or (or (kt-best-first (append posLi (list (first (sort_point posLi s)))) s (- m 1))
        (kt-best-first (append posLi (list (cadr (sort_point posLi s)))) s (- m 1)))
        (or (kt-best-first (append posLi (list (caddr (sort_point posLi s)))) s (- m 1))
        (kt-best-first (append posLi (list (list-ref (sort_point posLi s) 3))) s (- m 1))))
        (or (or (kt-best-first (append posLi (list (list-ref (sort_point posLi s) 4))) s (- m 1))
        (kt-best-first (append posLi (list (list-ref (sort_point posLi s) 5))) s (- m 1)))
        (or (kt-best-first (append posLi (list (list-ref (sort_point posLi s) 6))) s (- m 1))
        (kt-best-first (append posLi (list (last (sort_point posLi s)))) s (- m 1))))
        (= (length posLi) (* s s)))))

(define (kt-best-first-hquad point size maximum)
  (if (kt-best-first (list point) size maximum)
      (display "\n")
      "No knight’s tour exists!\n"))

(define (simple-check pos posLi s m)
  (cond 
    [(< m node) #f]
    [(member pos (cdr (reverse posLi))) (and (set! node (+ node 1)) #f)]
    [(> (car pos) (- s 1)) (and (set! node (+ node 1)) #f)]
    [(> (cadr pos) (- s 1)) (and (set! node (+ node 1))#f)]
    [(< (car pos) 0) (and (set! node (+ node 1)) #f)]
    [(< (cadr pos) 0) (and (set! node (+ node 1)) #f)]
    [else (and (+ node 1) #t)]))

(define (h listP posLi s m c) 
  (cond 
    [(equal? (length listP) 0) c]
    [(simple-check (car listP) posLi s m) (h (cdr listP) posLi s m (+ c 1))]
    [else (h (cdr listP) posLi s m c)]))

(define (num pos posLi s m)
  (sort (list (list(+ (car pos) 1) (+ (cadr pos) 2))
        (list (+ (car pos) 1) (- (cadr pos) 2))
        (list (- (car pos) 1) (+ (cadr pos) 2))
        (list (- (car pos) 1) (- (cadr pos) 2))
        (list (+ (car pos) 2) (+ (cadr pos) 1))
        (list (+ (car pos) 2) (- (cadr pos) 1))
        (list (- (car pos) 2) (+ (cadr pos) 1))
        (list (- (car pos) 2) (- (cadr pos) 1)))
        (lambda (pos1 pos2) (< (h (list (list(+ (car pos1) 1) (+ (cadr pos1) 2))
        (list (+ (car pos1) 1) (- (cadr pos1) 2))
        (list (- (car pos1) 1) (+ (cadr pos1) 2))
        (list (- (car pos1) 1) (- (cadr pos1) 2))
        (list (+ (car pos1) 2) (+ (cadr pos1) 1))
        (list (+ (car pos1) 2) (- (cadr pos1) 1))
        (list (- (car pos1) 2) (+ (cadr pos1) 1))
        (list (- (car pos1) 2) (- (cadr pos1) 1))) posLi s m 0) 
        (h (list (list(+ (car pos2) 1) (+ (cadr pos2) 2))
        (list (+ (car pos2) 1) (- (cadr pos2) 2))
        (list (- (car pos2) 1) (+ (cadr pos2) 2))
        (list (- (car pos2) 1) (- (cadr pos2) 2))
        (list (+ (car pos2) 2) (+ (cadr pos2) 1))
        (list (+ (car pos2) 2) (- (cadr pos2) 1))
        (list (- (car pos2) 2) (+ (cadr pos2) 1))
        (list (- (car pos2) 2) (- (cadr pos2) 1))) posLi s m 0)))))
    
(define (kt-best-first-hh posLi s m) ;position list, size, maximum
  (and (check (car (reverse posLi)) posLi s m) 
        (or (or (or (kt-best-first-hh (append posLi (list (first (num (car (reverse posLi)) posLi s m)))) s (- m 1))
        (kt-best-first-hh (append posLi (list (cadr (num (car (reverse posLi)) posLi s m)))) s (- m 1)))
        (or (kt-best-first-hh (append posLi (list (caddr (num (car (reverse posLi)) posLi s m)))) s (- m 1))
        (kt-best-first-hh (append posLi (list (list-ref (num (car (reverse posLi)) posLi s m) 3))) s (- m 1))))
        (or (or (kt-best-first-hh (append posLi (list (list-ref (num (car (reverse posLi)) posLi s m) 4))) s (- m 1))
        (kt-best-first-hh (append posLi (list (list-ref (num (car (reverse posLi)) posLi s m) 5))) s (- m 1)))
        (or (kt-best-first-hh (append posLi (list (list-ref (num (car (reverse posLi)) posLi s m) 6))) s (- m 1))
        (kt-best-first-hh (append posLi (list (last (num (car (reverse posLi)) posLi s m)))) s (- m 1))))
        (= (length posLi) (* s s)))))

(define (kt-best-first-h point size maximum)
  (if (kt-best-first-hh (list point) size maximum)
      (display "\n")
      "No knight’s tour exists!"))
(display (~a (list (random 5) (random 5)) "\n"))


(for ([i (in-range 0 10)])
  (kt-best-first-hquad (list (random 4) (random 4)) 5 1000000))

;(kt-best-first-hquad '(0 0) 5 1000000)
;(kt-best-first-h '(0 0) 5 1000000)

