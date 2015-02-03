(use postscheme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VECT
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SEGMENT
(define (make-segment sv ev)
  (cons sv ev))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FRAME
;;; origin edge1 edge2 は vect
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 (cons edge2 '()))))
(define (origin-frame frame)
  (first frame))
(define (edge1-frame frame)
  (second frame))
(define (edge2-frame frame)
  (third frame))
;;; フレーム を受け取り
;;; vector => vector' とする手続きを返す
;;; フレームによって変換を定義する?
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PAINTER
;;; painter は frame を受け取って描画
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))
;;; 上下反転
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ;新しいorigin
                     (make-vect 1.0 1.0)   ;edge1
                     (make-vect 0.0 0.0))) ;edge2
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
;;; 反時計回り90
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0) ;原点右下
                     (make-vect 1.0 1.0) ;edge1は右上
                     (make-vect 0.0 0.0)))
;;; 反時計回り180
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
;;; 反時計 270
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
;;; 右上に押し込め
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
;;; painter1 painter2
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
;;; painter2
;;; painter1
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 実際の描画
(define (draw-line v1 v2)
  (moveto (* 300 (xcor-vect v1)) (* 300 (ycor-vect v1)))
  (lineto (* 300 (xcor-vect v2)) (* 300 (ycor-vect v2)))
  (stroke))

(define-syntax picture
  (syntax-rules ()
    [(picture filename expression ...)
     (EPS filename 300 300
          (translate 10 10)
          expression
          ...)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST
(define normal-frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))
(define naname-frame (make-frame (make-vect 0.2 0.2) (make-vect 0 0.5) (make-vect 0.8 0.3)))

(define x-painter
  (segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 1 1))
                      (make-segment (make-vect 1 0) (make-vect 0 1)))))
(define outer-painter
  (segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 1 0))
                      (make-segment (make-vect 1 0) (make-vect 1 1))
                      (make-segment (make-vect 0 0) (make-vect 0 1))
                      (make-segment (make-vect 0 1) (make-vect 1 1)))))
(define hisi-painter
  (segments->painter (list
                      (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                      (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                      (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                      (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

(define b-painter
  (segments->painter (list
                      (make-segment (make-vect 0.1 0.1) (make-vect 0.5 0.1))
                      (make-segment (make-vect 0.1 0.1) (make-vect 0.1 0.9))
                      (make-segment (make-vect 0.5 0.1) (make-vect 0.5 0.5))
                      (make-segment (make-vect 0.1 0.5) (make-vect 0.5 0.5)))))
(define wave-painter
  (segments->painter 
   (list (make-segment (make-vect 0.25 0.00) (make-vect 0.37 0.37)) ;1
         (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.25)) ;2
         (make-segment (make-vect 0.50 0.25) (make-vect 0.62 0.00)) ;3
         (make-segment (make-vect 0.75 0.00) (make-vect 0.70 0.50)) ;4
         (make-segment (make-vect 0.70 0.50) (make-vect 1.00 0.30)) ;5
         (make-segment (make-vect 1.00 0.50) (make-vect 0.75 0.62)) ;6
         (make-segment (make-vect 0.75 0.62) (make-vect 0.62 0.62)) ;7
         (make-segment (make-vect 0.62 0.62) (make-vect 0.75 0.75)) ;8
         (make-segment (make-vect 0.75 0.75) (make-vect 0.62 1.00)) ;9
         (make-segment (make-vect 0.40 1.00) (make-vect 0.30 0.75)) ;10
         (make-segment (make-vect 0.30 0.75) (make-vect 0.40 0.62)) ;11
         (make-segment (make-vect 0.40 0.62) (make-vect 0.25 0.62)) ;12
         (make-segment (make-vect 0.25 0.62) (make-vect 0.20 0.50)) ;13
         (make-segment (make-vect 0.20 0.50) (make-vect 0.00 0.70)) ;14
         (make-segment (make-vect 0.37 0.37) (make-vect 0.30 0.50)) ;15
         (make-segment (make-vect 0.30 0.50) (make-vect 0.12 0.37)) ;16
         (make-segment (make-vect 0.12 0.37) (make-vect 0.00 0.50)) ;17
         (make-segment (make-vect 0.50 0.70) (make-vect 0.35 0.75)) ;smile 1
         (make-segment (make-vect 0.50 0.70) (make-vect 0.65 0.75)) ;smile 2
         )))
(define wave2 (beside wave-painter (flip-vert wave-painter)))

(picture "test.eps"
         ((corner-split wave-painter 5) normal-frame))



