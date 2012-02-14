#lang racket

(require htdp/image)
(provide (struct-out posn)
         (struct-out circuit-node)
         (struct-out connection)
         (struct-out short-circuit)
         (struct-out open-circuit)
         ternary->circuit
         random-circuit
         circuit->image
         circuit->bigimage
         image->circuit
         bigimage->circuit
         big-connections)

;---------------------------------------------------------------------------------------
; STRUCTS

(define-struct posn (x y) #:transparent)
(define-struct circuit-node (color) #:transparent)
(define-struct connection (pin1 pin2) #:transparent)
(define-struct short-circuit (pin) #:transparent)
(define-struct open-circuit (pin1 pin2) #:transparent)

;---------------------------------------------------------------------------------------
; CONNECTIONS LIST

(define big-connections
  (list (make-connection (make-posn 1 2) (make-posn 1 4))
        (make-connection (make-posn 1 4) (make-posn 1 6))
        (make-connection (make-posn 10 1) (make-posn 14 1))
        (make-connection (make-posn 10 1) (make-posn 8 6))
        (make-connection (make-posn 13 7) (make-posn 14 8))
        (make-connection (make-posn 14 8) (make-posn 19 7))
        (make-connection (make-posn 14 8) (make-posn 19 6))
        (make-connection (make-posn 19 7) (make-posn 19 6))
        (make-connection (make-posn 4 13) (make-posn 3 14))
        (make-connection (make-posn 13 23) (make-posn 13 24))
        (make-connection (make-posn 12 22) (make-posn 13 23))
        (make-connection (make-posn 4 13) (make-posn 13 23))
        ; start combinations
        (make-connection (make-posn 15 28) (make-posn 16 28))
        (make-connection (make-posn 15 28) (make-posn 17 25))
        (make-connection (make-posn 15 28) (make-posn 18 26))
        (make-connection (make-posn 15 28) (make-posn 18 23))
        (make-connection (make-posn 15 28) (make-posn 19 22))
        (make-connection (make-posn 15 28) (make-posn 19 15))

        (make-connection (make-posn 16 28) (make-posn 17 25))
        (make-connection (make-posn 16 28) (make-posn 18 26))
        (make-connection (make-posn 16 28) (make-posn 18 23))
        (make-connection (make-posn 16 28) (make-posn 19 22))
        (make-connection (make-posn 16 28) (make-posn 19 15))

        (make-connection (make-posn 17 25) (make-posn 18 26))
        (make-connection (make-posn 17 25) (make-posn 18 23))
        (make-connection (make-posn 17 25) (make-posn 19 22))
        (make-connection (make-posn 17 25) (make-posn 19 15))


        (make-connection (make-posn 18 26) (make-posn 18 23))
        (make-connection (make-posn 18 26) (make-posn 19 22))
        (make-connection (make-posn 18 26) (make-posn 19 15))

        (make-connection (make-posn 18 23) (make-posn 19 22))
        (make-connection (make-posn 18 23) (make-posn 19 15))

        (make-connection (make-posn 19 22) (make-posn 19 15))

        ; end combinations

        (make-connection (make-posn 4 13) (make-posn 2 29))
        (make-connection (make-posn 18 26) (make-posn 2 29))
        (make-connection (make-posn 15 28) (make-posn 2 29))
        (make-connection (make-posn 19 15) (make-posn 2 29))
        (make-connection (make-posn 0 13) (make-posn 8 6))))


;---------------------------------------------------------------------------------------
; PROVIDED FUNCTIONS

; list of list of int -> list of list of circuit-node
; transforms a grid of numbers into a circuit, where
; 0 is white, 1 is black, and 2 is blue
(define (ternary->circuit list)
  (map (lambda (lst)
         (map (lambda (x)
                (make-circuit-node (cond [(= x 0) 'white]
                                         [(= x 1) 'black]
                                         [(= x 2) 'blue])))
              lst))
       list))

; int, int, number, number -> list of list of circuit-node
; creates a random circuit of width x and height y
; conprob is the probability that a node will be conductive (black or blue)
; pinprob is the probability that a conductive node will be a pin (blue)
(define (random-circuit x y conprob pinprob)
  (build-list y (lambda (i1)
                  (build-list x (lambda (i)
                                  (make-circuit-node (cond
                                                       [(> (random) conprob) 'white]
                                                       [(> (random) pinprob) 'black]
                                                       [else 'blue])))))))

; list of list of circuit-node -> image
; transforms a circuit into an image where each node is 1 pixel
(define (circuit->image circuit)
  (circuit->image-n circuit 1))

; list of list of circuit-node -> image
; transforms a circuit into an image where each node is 10 pixels
(define (circuit->bigimage circuit)
  (circuit->image-n circuit 10))

; image -> list of list of circuit-node
; transforms an image into a circuit with one node for each pixel
(define (image->circuit image)
  (image->circuit-n (image->list2 image) 1 1))

; image -> list of list of circuit-node
; transforms an image into a circuit with one node for every 10 pixels
(define (bigimage->circuit image)
  (image->circuit-n (image->list2 image) 10 10))

;---------------------------------------------------------------------------------------
; HELPER FUNCTIONS

(define (circuit->image-n circuit n)
  (list2->image (circuit->image-helper circuit n)))

(define (circuit->image-helper circuit n)
  (foldr (lambda (row rest) (append (display-row row n) rest)) empty circuit))

(define (display-row circuit n)
  (if (empty? circuit)
      (build-list n (const empty))
      (map (lambda (lst)
             (let ([color
                    (cond
                      [(equal? 'black (circuit-node-color (first circuit))) (lambda (x) (make-color 0 0 0))]
                      [(equal? 'white (circuit-node-color (first circuit))) (lambda (x) (make-color 255 255 255))]
                      [(equal? 'blue (circuit-node-color (first circuit))) (lambda (x) (make-color 0 0 255))]
                      [(equal? 'red (circuit-node-color (first circuit))) (lambda (x) (make-color 255 0 0))])])
               (append (build-list n color) lst)))
           (display-row (rest circuit) n))))

(define (image->circuit-n image pin-size count)
  (cond
    [(empty? image) empty]
    [(empty? (first image)) (cons empty (image->circuit-n (rest image) pin-size 1))]
    [(= pin-size count) (cons (row->circuit (first image) pin-size pin-size)
                              (image->circuit-n (rest image) pin-size 1)) ]
    [else (image->circuit-n (rest image)
                                 pin-size
                                 (add1 count))]))

(define (row->circuit row pin-size count)
  (cond
    [(empty? row) '()]
    [(= pin-size count) (cons (make-circuit-node (cond
                                                   [(equal? (first row) (make-color 0 0 0)) 'black]
                                                   [(equal? (first row) (make-color 255 255 255)) 'white]
                                                   [(equal? (first row) (make-color 0 0 255)) 'blue]
                                                   [(equal? (first row) (make-color 255 0 0)) 'red]))
                              (row->circuit (rest row) pin-size 1))]
    [else (row->circuit (rest row) pin-size (+ 1 count))]))

(define (list2->image rows)
  (color-list->image (apply append rows)
                     (if (empty? rows) 0 (length (first rows)))
                     (length rows)
                     0 0))

(define (image->list2 image)
  (local ((define width (image-width image))
          (define (chunk lst)
            (if (empty? lst)
                empty
                (cons (take lst width)
                      (chunk (drop lst width))))))
    (chunk (image->color-list image))))
