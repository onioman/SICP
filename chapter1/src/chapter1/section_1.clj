(ns chapter1.section_1)

;exercise 1
;==========
(def ex1_1 10)
(def ex1_2 (+ 5 3 4))
(def ex1_3 (- 9 1))
(def ex1_4 (/ 6 2))
(def ex1_5 (+ (* 2 4) (- 4 6)))
(def ex1_a 3)
(def ex1_b (+ ex1_a 1))
(def ex1_6 (+ ex1_a ex1_b (* ex1_a ex1_b)))
(def ex1_7 (= ex1_a ex1_b))
(def ex1_8 
  (if (and (> ex1_b ex1_a) (< ex1_b (* ex1_a ex1_b))) ex1_b
                                                      ex1_a))
(def ex1_9
  (cond 
    (= ex1_a 4) 6
    (= ex1_b 4) (+ 6 7 ex1_a)
    :else 25))

(def ex1_10 (+ 2 (if (> ex1_b ex1_a) ex1_b ex1_a)))
(def ex1_11 
  (* (cond 
       (> ex1_a ex1_b) ex1_a
       (< ex1_a ex1_b) ex1_b
       :else -1)
     (+ ex1_a 1))) 

;exercise 2
;==========
(def ex2
  (/ (+ 5 4 2 -3 6 (/ 4 3))
     (* 3 (- 6 2) (- 2 7))))

;exercise 3
;==========
(defn ex3 [a b c]
  (let [sorted (sort (list a b c))]
    (+ (* (last sorted) (last sorted))
       (* (second sorted) (second sorted)))))

;exercise 4
;==========
(defn a-plus-abs-b [a b] ((if (pos? b) + -) a b))

;exercise 5
;==========
(defn p [] (p))

(defn order-test [x y]
  (if (zero? 0) 0 y))

(comment 
  (order-test 0 (p)))

;As Sebas says, applicative-order evaluation results in a stack overflow
;since p is infinetely recrusive. On the contrary, normal-order evaluation
;return immediatly with the result since p is never evaluated.

;square roots
;============
(defn square [x]
  (* x x))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (let [improved (improve guess x)]
    (if (good-enough? guess x) 
      guess
      (sqrt-iter improved x))))

(defn sqrt [x]
  (sqrt-iter 1.0 x))


;exercise 6
;==========
(defn new-if [predicate then-clause else-clause]
  (cond 
    predicate   then-clause
    :else       else-clause))

(defn new-sqrt-iter [guess x]
  (new-if
       (good-enough? guess x) guess 
                              (new-sqrt-iter (improve guess x) x)))

(defn new-sqrt [x]
  (new-sqrt-iter 1.0 x))

;it gets an stack overflow becase both then and else clauses are
;evaluated but I am not very sure why both are evaluated

;exercise 7
;==========
(defn small-variation? [guess improved]
  ;(let [diff (Math/abs (- guess improved))]
  ;  (< (/ diff guess) 0.0001)))
  (< (Math/abs (- (/ improved guess) 1)) 0.001))
(defn sqrt-iter-variation [guess x]
  (let [improved (improve guess x)]
    (if (small-variation? guess improved)
      guess
      (sqrt-iter improved x))))
(defn sqrt-variation [x]
  (sqrt-iter-variation 1.0 x))

; I don't see any change at all using the small-variation stop condition

;exercise 8
;==========
(defn cube-improve [guess x]
  (let [nume (+ (/ x (* guess guess)) (* 2 guess))]
    (/ nume 3)))

(defn cube [x] (* x x x))

(defn cube-good-enough? [guess improved x]
  (< (Math/abs (- (cube guess) x)) 0.001))

(defn cube-root-iter [guess x]
  (let [improved (cube-improve guess x)]
    (if (cube-good-enough? guess improved x)
      guess
      (cube-root-iter improved x))))

(defn cube-root [x]
  (cube-root-iter 1.0 x))


; Block structure
; ===============

(defn sqrt-block [x]
  (letfn [(good-enough? [guess improved]
            (< (Math/abs (- (/ improved guess) 1))
               0.001))
          (improve [guess]
            (average guess (- x guess)))
          (sqrt-iter [guess]
            (let [improved (improve guess)]
              (if (good-enough? guess improved)
                guess
                (sqrt-iter improved))))]
    (sqrt-iter 1.0)))

; Using the small-variation condition for stop here has a different
; result.
