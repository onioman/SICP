(ns chapter2.section_1
  (:use [chapter1.section_2 :only [prime? gcd]])
  (:use [chapter1.section_1 :only [sqrt new-sqrt]]))

(defn make-rat [n d] 
  (let [g (gcd n d)]
    [(/ n g) (/ d g)]))

(defn numer [x] (first x))
(defn denom [x] (last x))

; Rational numbers
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (println (str (numer x) "/" (denom x))))

; Exercise 2.1.1
;===============
(defn make-rat [n d] 
  (let [g (gcd n d)
        neg-g (- 0 g)]
    (cond
      (and (neg? n) (neg? d)) [(/ n neg-g) (/ d neg-g)]
      (neg? n)                [(/ n g) (/ d g)]
      (neg? d)                [(/ n neg-g) (/ d neg-g)]
      :else                   [(/ n g) (/ d g)])))

; Exercise 2.2
; ============
(defn make-point [x y] [x y])
(defn x-point [x] (first x))
(defn y-point [x] (last x))

(defn make-segment [start end] [start end])
(defn start-segment [x] (first x))
(defn end-segment [x] (last x))

(defn midpoint-segment [seg]
  (letfn [(avg [x y] (/ (+ x y) 2.0))]
    (let [start (start-segment seg)
          end   (end-segment seg)]
    (make-point (avg (x-point start) (x-point end))
                (avg (y-point start) (y-point end))))))

(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))
(defn print-segment [seg]
  (let [x1 (x-point (start-segment seg))
        y1 (y-point (start-segment seg))
        x2 (x-point (end-segment seg))
        y2 (y-point (end-segment seg))]
    (println (str "(" x1 "," y1 ") ----- (" x2 "," y2 ")")))) 

; Exercise 2.3
;=============
; rectangle as four points, will no check for angle correctnes
(defn make-rectangle [p1 p2 p3 p4]
  [p1 p2 p3 p4])

(defn cube [x] (* x x))

(defn length-segment [seg]
  (let [start (start-segment seg)
        end   (end-segment seg)]
    (sqrt (+ (cube (- (x-point start) (x-point end)))
             (cube (- (y-point start) (y-point end)))))))

(defn top-rect [rect]
  (make-segment (first rect) (nth rect 1)))
(defn right-rect [rect]
  (make-segment (nth rect 1) (nth rect 2)))
(defn bottom-rect [rect]
  (make-segment (nth rect 2) (nth rect 3)))
(defn left-rect [rect]
  (make-segment (nth rect 3) (first rect)))
(defn base-rect [rect]
  (length-segment (bottom-rect rect)))
(defn height-rect [rect]
  (length-segment (right-rect rect)))

(defn perimeter-rect [rect]
  (+ (length-segment (top-rect rect))
     (length-segment (bottom-rect rect))
     (length-segment (left-rect rect))
     (length-segment (right-rect rect))))
(defn area-rect [rect]
  (* (base-rect rect) (height-rect rect)))


; Exercise 2.4
;=============
(defn pair [x y]
  #(% x y))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

;(pair x y)
;F[x,y](f)

;(car (pair x y))
;(car F[x,y](f))
;(F[x,y](f(x,y)=x)

; Exercise 2.5
;=============
(defn factors-of [n]
  (letfn [(factor-of-iter [acum unprocessed]
               (if (zero? (rem unprocessed n))
                 (recur (inc acum) (/ unprocessed n))
                 acum))]
    (fn [x]
      (factor-of-iter 0 x))))

(defn int-pair [a b]
  (apply * (concat (repeat a 2) (repeat b 3))))

(defn int-car [x]
  ((factors-of 2) x))

(defn int-cdr [x]
  ((factors-of 3) x))

; Exercise 2.6
;==============
(defn church-zero [f]
  identity)

(defn church-add-1 [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

;(church-add-1 church-zero)
;(f ((church-zero f) x))
;(f (ident x))
;(f x)

(defn church-one [f]
  (fn [x]
    (f x)))

(defn church-two [f]
  (fn [x]
   (f (f x))))

(defn church-add [a b]
  (fn [f]
    (fn [x]
      ((a f) ((b f) x)))))
      ;((comp (a f) (b f)) x))))

; Exercise 2.7
;=============
(defn make-interval [a b] [a b])

(defn upper-bound [interval]
  (if (> (first interval) (last interval))
    (first interval)
    (last interval)))
(defn lower-bound [interval]
  (if (< (first interval) (last interval))
    (first interval)
    (last interval)))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(defn div-interval [x y]
  (mul-interval x (make-interval (/ 1.0 (upper-bound y)) 
                                 (/ 1.0 (lower-bound y)))))

; Exercise 2.8
;=============
(defn sub-interval [x y]
  (add-interval x (make-interval (- 0 (upper-bound y))
                                 (- 0 (lower-bound y)))))

; Exercise 2.9
; =============
(defn width-interval [interval]
  (/ (- (upper-bound interval) (lower-bound interval)) 2))
; TOOD

; Exercise 2.10
;==============
(defn div-interval [x y]
  (letfn [(spans-zero? []
          (<= (lower-bound y) 0 (upper-bound y)))]
    (if (spans-zero?)
      (println "ERROR: cant divied bt zero")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

; Exercise 2.11
;==============
;(defn mul-interval [x y]
  ;(cond
   ; (and (pos? lower-bound x) (pos? lower-bound y)) 1))  
; TODO


; Exercise 2.12
;==============
(defn make-center-percent [c p]
  (let [w (* (/ c 100) p)]
    (make-interval (- c w) (+ c w))))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn percent [i]
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))

; Exercise 2.13
;==============
; TODO: with paper

; Exercise 2.14
;==============
(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

;TODO: add explications
