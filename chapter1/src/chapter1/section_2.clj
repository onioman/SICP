(ns chapter1.section_2)

;execise 1.9
;============
(defn +-inc [a b]
  (if (= a 0)
    b
    (inc (+-inc (dec a) b))))

(defn +-dec [a b]
  (if (= a 0)
    b
    (+-dec (dec a) (inc b))))

; The one using inc is recursive since the final value is 
; obtain after applying inc to all the intermediate values
; (inc (inc (inc (inc .... b) ......)))
; The one using dec is iterative since the state is mantained in
; the variables a b through the iterations.

; exercise 1.10
;==============
(defn ackermann [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (ackermann (- x 1) (ackermann x (- y 1)))))

(comment
  (ackermann 1 10))
;1024

(comment
  (ackermann 2 4))
;65536

(comment
  (ackermann 3 3))
;65536

(defn f [n]
  (ackermann 0 n))

; f(n) = 2n

(defn g [n]
  (ackermann 1 n)
)
; g(n) = 2^n

(defn h [n] (ackermann 2 n))

; give an overflow for values n >= 5 but
; h(n) = 2^h(n-1)

;A(2,5) = A(1, A(2,4)) = A(1, ...
;A(2,4) = A(1, A(2,3)) = A(1, 16) = A(0, A(1,15) = 2*A(1,15)=2*2^15
;A(2,3) = A(1, A(2,2)) = A(1, 4) = A(0, A(1,3) = 2*A(1, 3) = 2*2*A(1,2)= 2*2*2*2  
;A(2,2) = A(1, A(2,1)) = A(1, 2) = A(0, A(1,1)) = 2*2
;A(2,1) = 2

; count-change example
;=====================
(defn first-denomination [kinds-of-coins]
  (cond
    (= kinds-of-coins 1) 1
    (= kinds-of-coins 2) 5
    (= kinds-of-coins 3) 10
    (= kinds-of-coins 4) 25
    (= kinds-of-coins 5) 50))

(defn cc [amount kinds-of-coins]
  (cond
    (= amount 0 )                           1
    (or (< amount 0) (= kinds-of-coins 0))  0
    :else (+ (cc amount (- kinds-of-coins 1))
             (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))

(defn count-change [amount]
  (cc amount 5))

; TODO: improve process

; exercise 1.11
;===============
(defn f-recursive [n]
  (if (< n 3) n
              (+ (f-recursive (- n 1)) 
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3))))))

(defn f-iterative-iter [a b c counter]
  (if (< counter 3) a
                    (f-iterative-iter 
                       (+ a (* 2 b) (* 3 c)) a b (- counter 1)))) 
(defn f-iterative [n]
  (f-iterative-iter 2 1 0 n))


; exercise 1.12
;===============

; testing some concepts
(defn next-line [line] 
  (let [more (rest line) head  (first line)] 
    (cond 
      (seq more) (cons (+ head 1) (next-line more))
      :else      (list ( + head 1))
    )))

(defn pascal-next-line [line]
 (let [one (first line) two (second line) more (rest line)]
  (if (and one two) (cons (+ one two) (pascal-next-line more))
                    '(1))))

(defn pascal-iter [line n]
  (let [next-line (cons 1 (pascal-next-line line))]
    ;(println next-line)
    (if (= n 1) (list line)
                (cons line (pascal-iter next-line (- n 1))))))

(defn pascal-nth-line [n]
  ;(println '(1))
  (pascal-iter '(1) n))

(defn pascal-pretty [n]
  (let [lines (pascal-nth-line n)]
    (dorun (map println lines))))

; exercise 1.13
;==============

; Fibonacci
(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1)   1
    :else     (+ (fib (- n 1)) fib (- n 2))))

; exercise 1.14
;==============
(defn pretty-cc [amount coins indent]
  (println (str indent "cc " amount " " (vec coins)))  
  (cond
    (zero? amount)              1
    (or (neg? amount)
        (zero? (count coins)))  0
    :else (+ (pretty-cc amount (rest coins) (str "  " indent))
             (pretty-cc (- amount (first coins)) coins indent))))

(defn pretty-change [n]
  (pretty-cc n [50 25 10 5 1] "  "))


; exericse 1.15
;==============
(defn cube [x] (* x x x))
(defn p [x] (- (* 3 x) (* 4 (cube x))))
(defn sine [angle]
  (if (not (> (Math/abs angle) 0.1)) 
    angle
    (p (sine (/ angle 3.0)))))

(defn sine-c [angle counter]
  (let [next-counter (inc counter)]
    (if (not (> (Math/abs angle) 0.1))
      (do 
        (println (str "After " next-counter " calls"))
        angle)
      (p (sine-c (/ angle 3.0) next-counter)))))
; a. 6
; b. 


; Exponentiation
;================
(defn expt-iter [b counter product]
  (if (= counter 0) product
                    (expt-iter b (- counter 1) (* b product))))

(defn expt [b n] (expt-iter b n 1))

(defn square [x] (* x x))
(defn fast-expt [b n]
  (cond (= n 0)   1
        (even? n) (square (fast-expt b (/ n 2)))
        :else     (* b (fast-expt b (- n 1)))))

; Exercise 1.16
;===============
(defn fast-exp [b n]
  (cond (zero? n) 1
        (even? n) (square (fast-exp b (/ n 2)))
        :else     (* b (fast-exp b (dec n)))))

(defn fast-iter-exp [b n]
  (loop [a 1, b b, n n]
    (cond
      (zero? n) a
      (even? n) (recur a (square b) (/ b 2))
      :else     (recur (* a b) b (dec b)))))

; Exercise 1.17
;===============
(defn mult [a b]
  (if (zero? b)
    0
    (+ a (* a (- b 1)))))

(defn hlv [x] (/ x 2))
(defn dbl [x] (* x 2))
(defn fast-mult [a b]
  (cond
    (zero? b) 0
    (even? b) (dbl (fast-mult a (hlv b)))
    :else     (+ a (fast-mult a (dec b)))))

; Exercise 1.18
;==============
(defn fast-iter-mult [a b]
  (loop [acum 0, a a, b b]
   (cond
    (zero? b) acum
    (even? b) (recur acum (dbl a) (hlv b))
    :else     (recur (+ acum a) a (dec b)))))


; Exercise 1.19
;==============

;demostration taken from Sebas

(defn fast-fib [n]
  (loop [a 1, b 0, p 0, q 1, n n]
    (cond
      (zero? n) b
      (even? n) (recur a b
                       (+ (square p) (square q))
                       (+ (* 2 p q) (square q))
                       (hlv n))
      :else     (recur (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p q (dec count)))))

; Exercise 1.20
;===============
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

; applicative
; (gcd 206 40) =
; (gcd 40 (mod 206 40)) = (gcd 40 6)
; (gcd 40 6) = (gcd 6 (mod 40 6)) = (gcd 6 4) =
; (gcd 6 4)  = (gcd 4 (mod 6 4)) = (gcd 4 2)
; (gcd 4 2)  = (gcd 2 (mod 4 2)) = (gcd 2 0)
;
;nomal
;(gcd 206 40) =
; (zero? 40) 
;(gcd 40 (mod 206 40)) =
; (zero? (mod 206 40)) ->
;(gcd (mod 206 40) (mod 40 (mod 206 40)))

; Exercise 1.21
(defn smallest-divisor [n]
  (loop [n n, test-div 2]
    (cond
      (> (square test-div) n)   n
      (zero? (mod n test-div))  test-div
      :else (recur n (inc test-div)))))

(comment
  (smaller-divisor 199))
;199

(comment
  (smaller-divisor 1999))
;1999

(comment
  (smaller-divisor 19999))
;7

; Exercise 1.22
;===============
(defn runtime [] (System/currentTimeMillis))

(defn timed-prime-test [n]
  (letfn [(prime? [n]
            (= (smallest-divisor n) n))]
    (let [start (runtime)]
      ; return a vector
      [n (prime? n) (- (runtime) start)])))
      ; return boolean + print
      ;(if (prime-test? n)
      ;  (do 
      ;    (println (str n " *** " (- (runtime) start)))
      ;    true)
      ;  (do
      ;    (println (str n " " (- (runtime) start)))
      ;    false)))))

(defn search-for-primes [start end]
  (let [search-space (range start end 2)]
    (filter second (map timed-prime-test search-space))))
    ;(filter timed-prime-test search-space)))


; Exercise 1.23
;===============
(defn timed-prime [prime-test n]
  (letfn [(prime? [n] (prime-test n))]
    (let [start (runtime)]
      [n (prime? n) (- (runtime) start)])))

(defn custom-smallest-div [next-div n]
  (loop [n n, test-div 2]
    (cond
      (> (square test-div) n)   n
      (zero? (mod n test-div))  test-div
      :else (recur n (next-div test-div)))))

(defn skip-div-of-two [n]
  (if (= n 2) 3 (+ n 2)))
  
(defn search-for-primes [start end condition]
  (let [search-space (range start end 2)]
    (filter second (map condition search-space))))

(defn fast-primes [start end]
  (search-for-primes start end
         (partial timed-prime #(= % (custom-smallest-div skip-div-of-two %))))) 

; Exercise 1.24
;==============

; Fermat thing
(defn expmod [base exp m]
  (cond
    (= exp 0)     1
    (even? exp)   (mod (square (expmod base (/ exp 2)  m))
                       m)
    :else         (mod (* base (expmod base (dec exp) m))
                       m)))


; Random integer calculation (taken from Sebas)
;(defn log2 [n]
;  (/ (Math/log n) (Math/log 2)))
;
;(defn rand-int [n]
;  (let [bits  (inc (int (Math/round (log2 n))))
;        rnd   (java.util.Random.)]
;    (loop []
;      (let [r (BigInteger. bits rnd)]
;       (if (< r n) r (recur)))))) 
;clojure has its own rand-int!

(defn fermat-test [n]
  (let [a (inc (rand-int (dec n)))]
    (= (expmod a n n) a)))

(defn fermat-prime? [times n]
  (cond (zero? times)   true
        (fermat-test n) (fermat-prime? (dec times) n)
        :else           false))

(defn fermat-prime-test [n]
  (timed-prime #(fermat-prime? 5 %) n))

(defn faster-primes [start end]
  (search-for-primes start end 
          (partial timed-prime (partial fermat-prime? 5))))

; Exercise 1.25
;==============
(defn fast-expmod [base exp m]
  (mod (fast-expt base exp) m))

; Exercise 1.26
;==============
; It happens because by using * it is calling recursevely twice to expmod.


; Exercise 1.27
;==============
(def carmichaels [561 1105 1729 2465 2821 6601])

(defn prime? [n]
  (= n (custom-smallest-div skip-div-of-two n)))

(defn probably-prime? [n]
  (every? #(= (expmod % n n) %) (range 1 n)))

(def fermat-says-prime-but-not-really
  (remove prime? (filter probably-prime? (range 7000))))

; Exercise 1.28
;==============
;TODO
