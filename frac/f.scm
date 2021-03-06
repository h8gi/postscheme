(use postscheme)
(define pi (acos -1))
(define (deg2rad deg)
  (* pi (/ deg 180)))
(define (_x point)
  (car point))
(define (_y point)
  (cdr point))
(define (point x y)
  (cons x y))
(define (vect x y)
  (cons x y))
(define (line p v)
  (cons p v))
(define (_p ln)
  (car ln))
(define (_v ln)
  (cdr ln))
(define (add-vect v0 v1)
  (vect (+ (_x v0)
           (_x v1))
        (+ (_y v0)
           (_y v1))))
(define (sub-vect v0 v1)
  (vect (- (_x v0)
           (_x v1))
        (- (_y v0)
           (_y v1))))
(define (dot-vect v0 v1)
  (+ (* (_x v0)
        (_x v1))
     (* (_y v0)
        (_y v1))))
(define (scale-vect a v0)
  (vect (* a (_x v0))
        (* a (_y v0))))
(define (rotate-vect theta v0)
  (vect (- (* (_x v0) (cos theta))
           (* (_y v0) (sin theta)))
        (+ (* (_x v0) (sin theta))
           (* (_y v0) (cos theta)))))

(define (koch-split ln)
  (let ((1/3vec (scale-vect 1/3 (_v ln)))
        (p0 (_p ln)))
    (list (line p0
                1/3vec)
          (line (add-vect p0 1/3vec)
                (rotate-vect (deg2rad 60)
                             1/3vec))
          (line (add-vect (add-vect p0 1/3vec)
                          (rotate-vect (deg2rad 60)
                                       1/3vec))
                (rotate-vect (deg2rad -60)
                             1/3vec))
          (line (add-vect p0
                          (scale-vect 2 1/3vec))
                1/3vec))))

(define (koch ln n)
  (define (inner llst count)
    (if (< count n)
        (inner (append-map koch-split llst) (+ 1 count))
        llst))
  (inner (list ln) 0))

(define (draw-line-list llst)
  (cond ((null? llst) 'done)
        (else
         (let ((ln (car llst)))
           (moveto (_x (_p ln))
                   (_y (_p ln)))
           (rlineto (_x (_v ln))
                    (_y (_v ln)))
           (stroke))
         (draw-line-list (cdr llst)))))

(define (digit n)
  (string-length (number->string n)))
(define (append-zero n dig)
  (let ((len (digit n)))
    (if (< len dig)
        (string-append (apply string (make-list (- dig len) #\0))
                       (number->string n)))))
(define (title-number.eps title number lim)
  (string-append title "-"
                 (append-zero number lim)
                 ".eps"))
(define (fill-canvas width height r g b)
  (gsave)
  (setrgbcolor r g b)
  (moveto 0 0)
  (rlineto width 0)
  (rlineto 0 height)
  (rlineto (- width) 0)
  (closepath)
  (fill)
  (grestore))
(define (drow-koch n)
  (let ((lim (+ (digit n) 1))
        (width 500)
        (height 500))
    (let loop ((count 0))
      (cond ((< count n)
             (EPS (title-number.eps "koch" count lim) width height
                  (fill-canvas width height 1 1 1)
                  (translate 30 (/ height 2.))
                  (setlinewidth 0.5)
                  (draw-line-list
                   (koch (line (vect 0. 0.) (vect 400. 0.)) count)))
             (loop (+ count 1)))
            (else 'done)))))

(drow-koch 8)

