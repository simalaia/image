#! /usr/bin/env scheme --script

;; Simple image library
;; https://rosettacode.org/wiki/Bitmap/Write_a_PPM_file (write image to file)
;; https://rosettacode.org/wiki/Bitmap  (create a simple bit map of an image)
;; https://rosettacode.org/wiki/Category:Raster_graphics_operations
;; https://rosettacode.org/wiki/Category:Image_processing
;; https://rosettacode.org/wiki/Category:Graphics_algorithms

(define-record-type pix (fields red green blue alpha))
(define pix make-pix)
(define (pix->bytevector p) (bytevector (pix-red p) (pix-green p) (pix-blue p)))
(define (pix-rgb p) `(,(pix-red p) ,(pix-green p) ,(pix-blue p)) )

;; http://homepages.inf.ed.ac.uk/rbf/HIPR2/arthops.htm
(define (alpha* a) (map (lambda (x) (* (pix-alpha a) x)) (pix-rgb a)))
(define (pix-over a b)
  (let ( (αa (pix-alpha a)) (αb (pix-alpha b)) (m (- 1 (pix-alpha a)))
	 (Ca (alpha* a)) (Cb (alpha* b)) )
    (cons (+ αa (* αb m)) (map + Ca (map (lambda (x) (* m x)) Cb))) ))


(define (pnt x y) `(,x ,y))
(define  pnt-x car)
(define  pnt-y cadr)
(define  pnt-    (case-lambda ((p) (map -          p)) ((a b) (map -   a b))))
(define  pnt-max (case-lambda ((p) (map max '(0 0) p)) ((a b) (map max a b))))
(define  pnt+    (lambda (a b)     (map +   a      b)))


;; img = (vec (vec pix ...) ...)
(define (make-image   px w h)
  (let ( (v (make-vector h)) )
    (let lop ( (n (- (vector-length v) 1)) )
      (cond
       ((< n 0) v )
       (else (vector-set! v n (make-vector w px)) (lop (- n 1)) )) )))
(define (image-width       s) (vector-length (vector-ref s 0)))
(define (image-height      s) (vector-length s))
(define (image-size        s) (* (surface-width s) (surface-height s)))
(define (image-ref      s pt) (vector-ref (vector-ref s (pnt-y pt)) (pnt-x pt)))
(define (image-set!  s px pt)
  (vector-set! (vector-ref s (pnt-y pt)) (pnt-x pt) px))
(define (image->list i)
  (let lop ( (l (vector->list i)) )
    (cond
     ((null? l) l )
     (else (append (vector->list (car l)) (lop (cdr l))) )) ))
(define (image->llist i)
  (let ( (pix->list (lambda (x) (bytevector->u8-list (pix->bytevector x)))) )
    (map (lambda (x) (map pix->list (vector->list x))) (vector->list i)))
  )
(define bounded?
  (case-lambda
    ((i p)
     (not (or (negative? (pnt-x p)) (negative? (pnt-y p))
	      (> (pnt-x p) (image-width i))
	      (> (pnt-y p) (image-height i)))) )
    ((i t p)
     (cond
      ((or (negative? (pnt-x p)) (negative? (pnt-y p))) #f )
      (else
       (if (pnt? t)
	   (bounded? i (pnt+ t p))
	   (bounded? i (pnt+ (pnt (image-width t)
				  image-height t) p))) )) )) )


;; File IO
(define (ppm-header w h m)
  (string->utf8
   (string-append "P6 "
		  (number->string w) " "
		  (number->string h) " "
		  (number->string m) "\n")) )
(define (ppm-write fn i)
  (let ( (fp (open-file-output-port fn)) (i (image->list i))
         (h (ppm-header (image-width i) (image-height i) 255)) )
    (put-bytevector fp h)
    (for-each (lambda (x) (put-bytevector fp (pix->bytevector x))) i) ))


;; Drawing primitives, todo: elipse, cubic/quadratic bezier curve
;; http://members.chello.at/~easyfilter/bresenham.html
(define (flood-fill i pt t r)
  (cond
   ((or (equal? t r) (not (equal? (image-ref i pt) t))) #f )
   (else (image-set! i r pt)
	 (flood-fill i (pnt+ pt (pnt  0 -1)) t r)
	 (flood-fill i (pnt+ pt (pnt  0  1)) t r)
	 (flood-fill i (pnt+ pt (pnt -1  0)) t r)
	 (flood-fill i (pnt+ pt (pnt  1  0)) t r) )) )

(define (draw-line i p a b) ;; (-> (img pix pnt pnt) img)
  (let ( (xa (pnt-x a)) (ya (pnt-y a)) (xb (pnt-x b)) (yb (pnt-y b)) )
    (let ( (dx (abs (- xb xa)))
	   (dy (- (abs (- yb ya))))
	   (sx (if (< xa xb) 1 -1))
	   (sy (if (< ya yb) 1 -1)) )
      (let lop ( (e (* 2 (+ dx dy))) (err (+ dx dy)) (x xa) (y ya) )
	(image-set! i p (pnt x y))
	(cond
	 ((and (= x xb) (= y yb)) #t )
	 ((>= e dy) (lop (* 2 (+ err dy)) (+ err dy) (+ x sx) y       ) )
	 ((<= e dx) (lop (* 2 (+ err dx)) (+ err dx) x        (+ y sy)) ))
	)) ))

(define (draw-circle  i p a r)  ;; (-> (img pix pnt nat) img)
  (define (setpix x y) (image-set! i p (pnt x y)) )
  (let ( (xa (pnt-x a)) (ya (pnt-y a)) )
    (let lop ( (x (- r)) (y 0) (err (- 2 (* 2 r))) (r r) ) ;; 2nd quadrant
      (setpix (- xa x) (+ ya y))  ;; 1st quadrant
      (setpix (- xa y) (- ya x))  ;; 2nd quadrant
      (setpix (+ xa x) (- ya y))  ;; 3rd quadrant
      (setpix (+ xa y) (+ ya x))  ;; 4th quadrant
      (let ( (r err) )
	(when (<= r y) (set! y (+ y 1)) (set! err (+ err (+ (* y 2)))))
	(when (or (> r x) (> err y))
	  (set! x (+ x 1)) (set! err (+ err (+ (* x 2) 1))))
	(when (< x 0) (lop x y err r)) )) ))

(define (draw-square i p o w h)
  (let* ( (x (pnt-x o)) (y (pnt-y o)) (X (+ x w)) (Y (+ y h)) )
    (draw-line i p (pnt x y) (pnt X y)) ;; horiz top
    (draw-line i p (pnt x Y) (pnt X Y)) ;; horiz bottom
    (draw-line i p (pnt x y) (pnt x Y)) ;; vert left
    (draw-line i p (pnt X y) (pnt X Y)) ;; vert right
    ))


;; Image overlay
(define (image-over b t o)
  (unless (bounded? b t o) (error "composition not bounded" "image-over") )
  (let lop ( (c (pnt 0 0)) )
    (cond
     ((not (image-bounded? t c)) #f )
     ((= (pix-alpha (image-ref t c)) 1)
      (image-set! b (image-ref t c) (pnt+ o c)) )) ))






;; Testing
(define (test)
  (define pix-transparent       (pix 255 255 255 0))
  (define pix-transparent-green (pix   0 255   0 0))
  (define pix-black             (pix   0   0   0 1))
  (define pix-green             (pix   0 255   0 1))
  (define pix-red               (pix 255   0   0 1))
  (define pix-orange            (pix 255 165   0 1))
  (define pix-purple            (pix 128   0 128 1))

  (define img     (make-image pix-green 100 100))
  (define v (make-image 0 3 3))
  (define centre (pnt (/ (image-width img) 2) (/ (image-height img) 2)))

  ;;(draw-circle img pix-black centre 25)
  (draw-circle img pix-red centre 35)
  (flood-fill  img centre pix-green pix-red)
  (draw-line   img pix-black (pnt 5 55) (pnt 95 55))
  (draw-line   img pix-black (pnt 5 45) (pnt 95 45))
  (flood-fill  img centre pix-red pix-black)
  ;;(flood-fill  img (pnt+ centre (pnt -4 -30)) pix-green pix-orange)
  ;;(flood-fill  img (pnt+ centre (pnt  0  30)) pix-green pix-purple)

  (ppm-write "try.ppm" img)
  )
(test)


;; **** compose/overlay images
;; alpha compositing
;; https://en.wikipedia.org/wiki/Alpha_channel
;; https://keithp.com/~keithp/porterduff/p253-porter.pdf
;; https://www.cs.princeton.edu/courses/archive/fall00/cs426/papers/smith95a.pdf
;; http://dvd-hq.info/alpha_matting.php
;; [NOTE]: Fix, I think I broke this.
#|
(define (resize b t o)
  (let ( (nsize (lambda (b t o) (pnt-max b (pnt+ t o))))
	 (max0  (lambda (b o)   (pnt+ b (pnt-max '(0 0) (pnt-neg o)))))
	 (max1  (lambda (o)     (pnt-max '(0 0) (pnt-neg o)) )) )
    (cond
     ((pnt< o '(0 0))   `(#f ,(max0 b o)    ,(max1 o) ,(pnt-max '(0 0) o)) )
     ((pnt< b (+ t o)) `(#f ,(nsize b t o) '(0 0) ,o) )
     (else             `(#t ,b ,o) )) ))
|#
;; **** equality
;; https://stackoverflow.com/questions/843972/image-comparison-fast-algorithm
;; (image=? a b) (-> (img img) bool)

