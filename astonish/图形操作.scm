(define device
  (make-graphics-device
   (car (enumerate-graphics-types))))
(define (draw-line start end)
  (graphics-draw-line device
                      (xcor-vect start) (ycor-vect start)
                      (xcor-vect end) (ycor-vect end)))
(define (for-each proc seq)
  (if (null? seq)
      #t
      (begin
        (proc (car seq))
        (for-each proc (cdr seq)))))
(define (make-vect x y)
  (list x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cadr v))
(define (add-vect u v)
  (make-vect
   (+ (xcor-vect u) (xcor-vect v))
   (+ (ycor-vect u) (ycor-vect v))))
(define (sub-vect u v)
  (make-vect
   (- (xcor-vect u) (xcor-vect v))
   (- (ycor-vect u) (ycor-vect v))))
(define (scale-vect scale v)
  (make-vect
   (* scale (xcor-vect v))
   (* scale (ycor-vect v))))
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))
(define (make-segment start end)
  (list start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
(define block
  (list
   (list (list 0 0.5) (list 0.5 0.5))
   (list (list 0.5 0.5) (list 0.5 0))
   (list (list 0.5 0)(list 0 0))
   (list (list 0 0) (list 0 0.5))))
(define standard-frame (make-frame (make-vect -1 -1)
                                   (make-vect 2 0)
                                   (make-vect 0 2)))
(define frame1
  (list
   (list -1 -1)
   (list 1 0)
   (list 1 2)))
;2.49
;;a
(define (border-painter frame)
  (define border
    (list
     (list (list 0 0) (list 1 0))
     (list (list 1 0) (list 1 1))
     (list (list 1 1) (list 0 1))
     (list (list 0 1) (list 0 0))))
  ((segments->painter border) frame))
;;b
(define (cross-painter frame)
  (define cross
    (list
     (list (list 0 0) (list 1 1))
     (list (list 0 1) (list 1 0))))
  ((segments->painter cross) frame))
;;c
(define (rhombus-painter frame)
  (define rhombus
    (list
     (list (list 0.5 0) (list 1 0.5))
     (list (list 1 0.5) (list 0.5 1))
     (list (list 0.5 1) (list 0 0.5))
     (list (list 0 0.5) (list 0.5 0))))
  ((segments->painter rhombus) frame))
(define (right-arrow-painters frame)
  (define arrow
    (list
     (list (list 0 0.5) (list 1 0.5))
     (list (list 0.7 0.7) (list 1 0.5))
     (list (list 0.7 0.3) (list 1 0.5))))
  ((segments->painter arrow)frame))
(define (up-arrow-painters frame)
  (define arrow
    (list
     (list (list 0.5 0) (list 0.5 1))
     (list (list 0.3 0.7) (list 0.5 1))
     (list (list 0.7 0.7) (list 0.5 1))))
  ((segments->painter arrow) frame))
;
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(define (beside painter1 painter2)
  (lambda (frame)
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0 0)
                              (make-vect 0.5 0)
                              (make-vect 0 1)))
          (paint-right
           (transform-painter painter2
                              (make-vect 0.5 0)
                              (make-vect 1 0)
                              (make-vect 0.5 1))))
      (paint-left frame)
      (paint-right frame))))
;2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
;2.51
(define (below painter1 painter2)
  (lambda (frame)
    (let ((paint-top (transform-painter painter2
                                       (make-vect 0 0.5)
                                       (make-vect 1 0.5)
                                       (make-vect 0 1)))
          (paint-below (transform-painter painter1
                                          (make-vect 0 0)
                                          (make-vect 1 0)
                                          (make-vect 0 0.5))))
      (paint-below frame)
      (paint-top frame))))
((flip-vert up-arrow-painters) frame1)
(up-arrow-painters frame1)
((shrink-to-upper-right up-arrow-painters) frame1)
((rotate90 up-arrow-painters) standard-frame)
((squash-inwards up-arrow-painters)standard-frame)
((squash-inwards border-painter) standard-frame)
(border-painter frame1)
((beside up-arrow-painters up-arrow-painters) standard-frame)
((flip-horiz right-arrow-painters) standard-frame)
((below right-arrow-painters right-arrow-painters) standard-frame)
