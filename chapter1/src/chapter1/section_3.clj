(ns chapter1.section_3
  (:use [chapter1.section_2 :only [prime? gcd]]))
  ;(:require clojure.contrib.generic.math-functions))

; Exercise 1.29
;==============
(defn sum [term a next-val b]
  (if (> a b) 0
              (+ (term a) (sum term (next-val a) next-val b))))
(defn integral [f a b dx]
  (* (sum f (+ a (/ dx 2.0)) (partial + dx) b) dx))

; term-k = (f (+ a (*k h)) where h = (/ (- b a) n)
; k = 0 .. n

(defn simpson-integral [f a b n]
  (let [even-n  (if (even? n) n (inc n))
        h       (/ (- b a) even-n)]
   (letfn [(term [i]
            (let [factor (cond 
                           (or (zero? i) (= i even-n))  1
                           (zero? (mod i 2))            2
                           :else                        4)
                  x      (+ a (* i h))]
              (* factor (f x))))]
     (* (sum term 0 inc even-n) (/ h 3)))))

;user=> (integral cube 0 1 0.001)            
;0.249999875000001

;user=> (simpson-integral cube 0 1 10) 
;1/4

; Exercise 1.30
;==============
(defn sum-iter [term a next-val b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (next-val a) (+ result (term a)))))]
   (iter a 0)))

; Exercise 1.31
;==============
(defn prod [term a next-val b]
  (if (> a b) 1
              (* (term a) (prod (next-val a) next-val b))))

(defn prod-iter [term a next-val b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (next-val a) (* result (term a)))))]
    (iter a 1)))

(defn factorial [n]
  (letfn [(ident [x] x)]
    (prod-iter ident 1 inc n)))

(defn wallis-next [n]
  (let [remainder (mod n 2)]
    (cond
      (zero? n)         (/ 2 3)
      (zero? remainder) (/ (* 2 (inc (/ n 2))) 
                           (+ 3 (* 2 (/ n 2))))
      :else             (/ (* 2 (inc (/ (inc n) 2)))
                           (+ 3 (* 2 (/ (dec n) 2)))))))

(defn wallis-phi [n]
  (* (prod-iter wallis-next 0 inc n) 4)) 

; Exercise 1.32
;==============
(defn accumulate [combiner null-val term a next-val b]
  (if (> a b) 
    null-val
    (combiner (term a) (accumulate (next-val a) next-val b))))

(defn accumulate-iter [combiner null-val term a next-val b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (iter (next-val a) (combiner result (term a)))))]
    (iter a null-val)))

(defn fact-acum [n]
  (letfn [(ident [x] x)]
    (accumulate-iter * 1 ident 1 inc n)))

; Exercise 1.33
;==============
(defn filtered-acum [combiner null-val pred term a next-val b]
  (letfn [(iter [a result]
            (let [filtered (if (pred a) (term a) null-val)]
              (if (> a b)
                result
                (iter (next-val a) (combiner result filtered)))))]
    (iter a null-val)))

;a)
(defn sum-square-primes [a b]
  (filtered-acum + 0 prime? #(* % %) a inc b)) 

;b)
(defn prod-relative-primes [n]
  (letfn [(ident [x] x)]
    (filtered-acum * 1 #(= 1 (gcd % n)) ident 1 inc n)))

; Exercise 1.34
;==============
(defn f3 [g]
  (g 2))

(comment
  (f3 #(* % %)))

(comment
  (f3 f3))

; It fails since we are trying to 2 as a function:
; (f f) = (f (f 2)) = (f (2 2)) 

; Half interval method
(defn abs [x]
  (if (neg? x)  (- 0 x) x))

(defn search [f neg-p pos-p]
  (letfn [(close-enough? [x y]
            (< (abs (- x y)) 0.001))]
    (let [mid-p (/ (+ neg-p pos-p) 2)
          test-val (f mid-p)]
      (if (close-enough? neg-p pos-p) 
        mid-p
        (cond (pos? test-val) (search f neg-p mid-p)
              (neg? test-val) (search f mid-p pos-p)
              :else           mid-p)))))

(defn half-interval-method [f a b]
  (let [a-val (f a)
        b-val (f b)]
    (cond (and (neg? a-val) (pos? b-val)) (search f a b)
          (and (neg? b-val) (pos? a-val)) (search f b a)
          :else             (print "Values are not opposite sign"))))

(comment
  (half-interval-method sin 2.0 4.0))

(comment
  (half-interval-method #(- (* % % %) (* 2 %) 3) 1.0 2.0))
;1.89306640625

; Fixed points
(def tolerance 0.00001)
(defn fixed-point [f first-guess]
  (letfn [(close-enough? [x y]
            (< (Math/abs (- x y)) tolerance))
          (try-it [guess]
            (let [guess-val (f guess)]
              (if (close-enough? guess guess-val)
                guess-val
                (try-it guess-val))))]
    (try-it first-guess)))

; TO TEST

; Exercise 1.35
;==============
;TODO: Proof!

(defn phi []
  (fixed-point #(+ 1 (/ 1 %)) 1.0))

;user=> (phi)                                
;1.6180327868852458

; Exercise 1.36
;==============
(defn fixed-point-p [f first-guess p]
  (letfn [(close-enough? [x y]
            (< (Math/abs (- x y)) tolerance))
          (try-it [guess]
            (let [guess-val (f guess)]
              (p guess guess-val)
              (if (close-enough? guess guess-val)
                guess-val
                (try-it guess-val))))]
    (try-it first-guess)))

(defn print-guess [guess next]
  (println "guess->" guess " next->" next))

(defn phi-p []
  (fixed-point-p #(+ 1 (/ 1 %)) 1.0 print-guess))

(defn x-pow-x []
  (fixed-point-p #(/ (Math/log 1000) (Math/log %)) 2.0 print-guess))

(defn average [x y]
  (/ (+ x y) 2))

(defn x-pow-x-avg []
  (fixed-point-p #(average (/ (Math/log 1000) (Math/log %)) %) 2.0 print-guess))


; Exercise 1.37
;==============
(defn cont-frac [n-fn d-fn k]
  (letfn [(cont-frac-aux [k-i k]
            (if (= k-i k)
              0
              (/ (n-fn k-i) (+ (d-fn k-i) (cont-frac-aux (inc k-i) k)))))]
    (cont-frac-aux 0 k)))
 
; TODO: iter
;N1 / D1
;N1 / D1 + 1 / N2 / D2
;(defn cont-frac-iter [n-fn

; Exercise 1.38
;==============
(defn d-euler [n]
  (let [factor (/ (- (dec n) (mod (dec n) 3)) 3)]
  (cond
    (zero? n)   1
    (= 1 n)     2
    :else       (if (zero? (mod (dec n) 3)) (* 2 (inc factor)) 1))))

(defn e-aprox [k]
  (+ 2 (cont-frac (fn [n] 1) d-euler k)))

; Exercise 1.39
;==============
(defn general-cont-frac [combine n-fn d-fn k]
  (letfn [(cont-frac-aux [k-i k]
            (if (= k-i k)
              0
              (/ (n-fn k-i) (combine (d-fn k-i) (cont-frac-aux (inc k-i) k)))))]
    (cont-frac-aux 0 k)))

(defn n-lambert [x k]
  (if (zero? k) x (* x x)))
(defn d-lambert [k]
  (+ 1 (* 2 k)))

(defn tan-approx [x k]
  (general-cont-frac - (partial n-lambert x) d-lambert k))

; Exercise 1.40
;==============
(def dx 0.00001)

(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))
(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn cubic [a b c]
  (fn [x]
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

; Exercise 1.41
;==============
(defn double-it [g]
  (fn [x] (g (g x))))

;user=> (((double-it (double-it double-it)) inc) 5)
;21

; Exercise 1.42
;==============
(defn compose [f g]
  (fn [x] (f (g x))))

;user=> ((compose #(* % %) inc) 6)
;49

; Exercise 1.43
;==============
(defn f-nth [f n]
  (letfn ([f-nth-iter [accum i]
           (if (= i n)  
             accum
             (recur (compose f accum) (inc i)))])
    (f-nth-iter (fn [x] (f x)) 1)))

; Exercise 1.44
;==============
(defn f-smooth [f]
  (fn [x]
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx))) 3)))

(defn f-smooth-nth [f n]
  (letfn ([iter [accum i]
           (if (= i n)
             accum
             (recur (compose f accum) (inc i)))])
    (iter (fn [x] (f x)) 1)))

; Exercise 1.45
; =============
; TODO

; Exercise 1.46
;==============
(defn iterative-improve [improve good-enough?]
  (fn [x]
    (if (good-enough? x)
      x
      (recur (improve x)))))

(defn sqrt-improve [x]
  ((iterative-improve #(/ (+ % (/ x %)) 2.0) #(< (abs (- (* % %) x)) 0.001)) x))
