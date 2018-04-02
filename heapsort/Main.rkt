#lang racket
(require pict)
(require pict/tree-layout)
(define-struct tree (value left right))
(define-struct heap ([size #:mutable] value))

(define (build-tree iter array)
  (define index (- iter 1))
  (define index21 (- (+ 1 (* iter 2)) 1))
  (define index2 (- (* iter 2) 1))
  (if
    (>= index (length array)) #f 
        (make-tree (list-ref array index) (build-tree (* 2 iter) array)
        (build-tree (+ 1 (* 2 iter)) array))))

(define (ctext n) (cc-superimpose  (filled-ellipse #:color "white" 15 15) (text n)))
(define (tree-show tree)
  (define (left t) (if (tree? t) (tree-left t) t))
  (define (right t) (if (tree? t) (tree-right t) t))
  (define val (if (tree? tree) (tree-value tree) tree))
  (define n (if val (ctext (number->string val)) #f))
  (if (tree? tree)
  (tree-layout #:pict n (tree-show (left tree)) (tree-show (right tree)))
  #f))

(define (draw-array array)
  (define tree (build-tree 1 array))
  (naive-layered (tree-show tree)))

(define (draw-vector vector)
  (define tree (build-tree 1 (vector->list vector)))
  (naive-layered (tree-show tree)))

(define (parent i) (floor (/ i 2)))

(define (left i) (* 2 i))

(define (right i) (+ 1 (* 2 i)))

(define (swap i j heap)
  (define array (heap-value heap))
  (define temp (vector-ref array (- i 1)))
  (begin (vector-set! array (- i 1) (vector-ref array (- j 1)))
         (vector-set! array (- j 1) temp)))

(define (heapify heap i)
  (let* (
          [array (heap-value heap)]
          [l (left i)]
          [r (right i)]
          [largest (if (and (<= l (heap-size heap))
                            (> (vector-ref array (- l 1))
                               (vector-ref array (- i 1))))
                       l i)]
          [largest (if (and (<= r (heap-size heap))
                            (> (vector-ref array (- r 1))
                               (vector-ref array (- largest 1))))
                       r largest)])
     (if (not (equal? largest i)) (begin
                              (swap i largest heap)
                              (heapify heap largest)) (void))))
(define (build-max-heap array)
  (define heap (make-heap (length array) (list->vector array)))
  (begin
    (for ([i (in-range (floor (/ (length array) 2)) 0 -1)])
      (heapify heap i))
    heap))

(define (heapsort array)
  (define heap (build-max-heap array))
  (begin
    (print (draw-vector (heap-value heap)))
    (for ([i (in-range (length array) 1 -1)])
      (begin
        (swap 1 i heap)
        (set-heap-size! heap (- (heap-size heap) 1))
        (heapify heap 1)
        (print (draw-vector (heap-value heap)))))
    (heap-value heap)))
  
                         
(define h (make-heap 3 (list->vector '(1 2 3))))
(heapsort '(1 2 3 4))
  
#|(define (draw-tree tree)
  (naive-layered|#
