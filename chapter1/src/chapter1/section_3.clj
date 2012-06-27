(ns chapter1.section_3
  (:use [chapter1.section_2 :only [prime? gcd]]))

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

