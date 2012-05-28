(ns chapter1.section_1)

;exercise 1
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
(def ex2
  (/ (+ 5 4 2 -3 6 (/ 4 3))
     (* 3 (- 6 2) (- 2 7))))

;exercise 3
(defn ex3 [a b c]
  (let [sorted (sort (list a b c))]
    (+ (* (last sorted) (last sorted))
       (* (second sorted) (second sorted)))))

;exercise 4
(defn a-plus-abs-b [a b] ((if (> b 0) + -) a b))

;exercise 5
;?????

;square roots
(defn abs [x]
  (if (> x 0) x (- 0 x)))

(defn square [x]
  (* x x))

(defn small-variation? [guess improved]
  (< (abs (- guess improved)) 0.00000001))

(defn good-enough? [guess improved x]
  (or (small-variation? guess improved)
      (< (abs (- (square guess) x)) 0.001)
  )
  ;(< (abs (- (square guess) x)) 0.001)
)

(defn average [x y]
  (/ (+ x y) 2)
)

(defn improve [guess x]
  (average guess (/ x guess))
)

(defn sqrt-iter [guess x]
  (let [improved (improve guess x)]
    (if (good-enough? guess improved x) 
      guess
      (sqrt-iter improved x)
    )
  )
)

(defn sqrt [x]
  (sqrt-iter 1.0 x)
)

;exercise 6V
(defn new-if [predicate then-clause else-clause]
  (cond 
    predicate   then-clause
    :else       else-clause
  )
)

(defn new_sqrt_iter [guess x]
  (new-if (good-enough? guess x) guess (new_sqrt_iter (improve guess x) x))
)

(defn new_sqrt [x]
  (new_sqrt_iter 1.0 x)
)

(defn cube-improve [guess x]
  (let [nume (+ (/ x (* guess guess)) (* 2 guess))]
    (/ nume 3)
  )
)

(defn cube [x]
  (* x x x))

(defn cube-good-enough? [guess improved x]
  ;(or (small-variation? guess improved)
  ;    (< (abs (- (square guess) x)) 0.001)
  ;)
  (< (abs (- (cube guess) x)) 0.001)
)

(defn cube-root-iter [guess x]
  (let [improved (cube-improve guess x)]
    (if (cube-good-enough? guess improved x)
      guess
      (cube-root-iter improved x)
    )
  )
)

(defn cubert [x]
  (cube-root-iter 1.0 x)
)


; execise 1.9

; exercise 1.10
(defn A [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1) (A x (- y 1)))
  )
)

(defn f [n]
  (A 0 n)
)

(defn g [n]
  (A 1 n)
)

(defn h [n] (A 2 n))
