;;; postscheme
(module postscheme
  *
  (import scheme chicken extras posix)
;;; Stack Operating
  (define (push . objs)
    (for-each (lambda (x) (display x) (display " ")) objs))
  (define (pop)
    (display "pop\n"))
  (define (dup)
    (display "dup\n"))
  (define (exch)
    (display "exch\n"))
  (define (clear)
    (display "clear\n"))

;;; Patch Construction
  (define (newpath)
    (display "newpath\n"))
  (define (closepath)
    (display "closepath\n"))
  (define (moveto x y)
    (printf "~a ~a moveto\n" x y))
  (define (rmoveto x y)
    (printf "~a ~a rmoveto\n" x y))
  (define (lineto x y)
    (printf "~a ~a lineto\n" x y))
  (define (rlineto x y)
    (printf "~a ~a rlineto\n" x y))
  (define (arc x y r ang1 ang2)
    (printf "~a ~a ~a ~a ~a arc\n" x y r ang1 ang2))
  (define (arcn x y r ang1 ang2)
    (printf "~a ~a ~a ~a ~a arcn\n" x y r ang1 ang2))
  (define (arcto x1 y1 x2 y2 r)           ;返り値あり
    (printf "~a ~a ~a ~a ~a arcto\n" x1 y1 x2 y2 r)
    (cons (- y1 r) (+ x1 r)))
  (define (currentpoint)                  ;返り値あり
    (display "currentpoint\n"))

;;; Painting Operators
  (define (stroke)
    (display "stroke\n"))
  (define (fill)
    (display "fill\n"))
  (define (setlinewidth width)
    (printf "~a setlinewidth\n" width))
  (define (setgray g)
    (printf "~a setgray\n" g))
  (define (setrgbcolor r g b)
    (printf "~a ~a ~a setrgbcolor\n" r g b))
;;; Dictionary Operators
  (define-syntax def
    (syntax-rules ()
      [(def (name) expression ...)
       (begin (printf "/~a {\n" (symbol->string 'name))
              expression ...
              (printf " } def\n")
              (define (name)
                (print (symbol->string 'name))))]
      [(def name expression ...)
       (begin (printf "/~a " (symbol->string 'name))
              expression ...
              (printf " def\n")
              (define (name)
                (print (symbol->string 'name))))]))
;;; Character and Font Operators
  (define (findfont font)
    (printf "/~a findfont\n" font))
  (define (scalefont size)
    (printf "~a scalefont\n" size))
  (define (setfont)
    (display "setfont\n"))
  (define (easy-fontset font size)
    (findfont font)
    (scalefont size)
    (setfont))
  (define (show str)
    (printf "(~a) show\n" str))
  (define (stringwidth str)               ;返り値あり
    (printf "(~a) stringwidth\n" str))

;;; Control Operators
  (define-syntax repeat                   ;素直にrepeat
    (syntax-rules ()
      [(repeat times body ...)
       (begin (display times)
              (display " {\n")
              body ...
              (display "} ")
              (display "repeat\n"))]))

  (define-syntax for  ;; これは上手くないけどスタックの上の物を使うよりは?
    (syntax-rules (in)
      [(for x in (start step end) proc)
       (do ((x start (+ x 1)))
           ((> x end))
         proc)]))

;;; Coordinate Systems Operators
  (define (translate x y)
    (printf "~a ~a translate\n" x y))
  (define (rotate deg)
    (printf "~a rotate\n" deg))
  (define (scale x y)
    (printf "~a ~a scale\n" x y))

;;; Graphics State Operators
  (define (gsave)
    (display "gsave\n"))
  (define (grestore)
    (display "grestore\n"))

;;; IMAGE
  (define (image len lines bits-per-sample transform-matrix proc-string)
    (printf "~a ~a ~a " len lines bits-per-sample)
    (printf "~? {<~a>} image\n"
            "[~a ~a ~a ~a ~a ~a]"
            transform-matrix
            proc-string))


  (define (ps-begin)
    (display "begin\n"))
  (define (ps-end)
    (display "end\n"))
;;; Output Operators
  (define (showpage)
    (display "showpage\n"))


;;; Main
  (define-syntax EPS
    (syntax-rules ()
      [(EPS filename width height expression ...)
       (call-with-output-file filename
         (lambda (out)
           (let ((tmp (current-output-port)))
             (current-output-port out)
             (display "%!EPS-Adobe-3.0 EPSF-3.0\n")
             (display "%%DocumentFonts: Helvetica\n")
             (printf "%%BoundingBox: 0 0 ~a ~a\n" width height)
             (display "%%Orientation: Portrait\n")
             (printf "%%Title: ~a\n" filename )
             (display "%%Pages: 1\n")
             (display "%%EndComments\n")
             (display "/mydict 120 dict def\nmydict begin\n")
             (display "gsave\n")
             expression ...
             (display "end\n")
             (display "grestore\n")
             (display "showpage\n")
             (display "%%Trailer\n")
             (current-output-port tmp)
             'done)))]))
  ;; (define-syntax EPS
  ;;   (ir-macro-transformer
  ;;    (lambda (exp inject compare)
  ;;      (let ((filename (cadr exp))
  ;;            (w (caddr exp))
  ;;            (h (cadddr exp))
  ;;            (exps (cddddr exp))
  ;;            (width (inject 'width))
  ;;            (height (inject 'height)))
  ;;        `(let ((,width ,w) (,height ,h))
  ;;           (call-with-output-file ,filename
  ;;             (lambda (out)
  ;;               (let ((tmp (current-output-port)))
  ;;                 (current-output-port out)
  ;;                 (display "%!EPS-Adobe-3.0 EPSF-3.0\n")
  ;;                 (display "%%DocumentFonts: Helvetica\n")
  ;;                 (printf "%%BoundingBox: 0 0 ~a ~a\n" ,width ,height)
  ;;                 (display "%%Orientation: Portrait\n")
  ;;                 (printf "%%Title: ~a\n" ,filename)
  ;;                 (display "%%Pages: 1\n")
  ;;                 (display "%%EndComments\n")
  ;;                 (display "/mydict 120 dict def\nmydict begin\n")
  ;;                 (display "gsave\n")
  ;;                 ,@exps
  ;;                 (display "end\n")
  ;;                 (display "grestore\n")
  ;;                 (display "showpage\n")
  ;;                 (display "%%Trailer\n")
  ;;                 (current-output-port tmp)
  ;;                 'done))))))))

  )
