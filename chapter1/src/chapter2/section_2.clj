(ns chapter2.section_2
  (:use [chapter1.section_2 :only [prime?]]))


; Exercise 2.17
; =============

(defn last-pair [l]
  (if (seq (rest l)) 
    (last-pair (rest l))
    (first l)))

; Exercise 2.18
; =============
(defn my-reverse [l]
  (letfn [(iter [acum l]
            (if (seq l)
              (recur (cons (first l) acum) (rest l))
              acum))]
    (iter [] l)))

; Exercise 2.19
; =============
(defn no-more? [coins]
  (not (seq coins)))
(defn except-first-denomination [coins]
  (rest coins))
(defn this-first-denomination [coins]
  (first coins))

(defn this-cc [amount coin-values]
  (cond 
    (zero? amount) 1
    (or (< amount 0) (no-more? coin-values)) 0
    :else (+ (this-cc amount (except-first-denomination coin-values))
             (this-cc (- amount (this-first-denomination coin-values)) coin-values)))) 

; As the implementation without lists, the order does not matter since all the
; combinations are calculated.

; Exersice 2.20
; =============
(defn same-parity [guide & other]
  (let [parity (if (even? guide) even? odd?)]
    (cons guide (filter parity other))))

; Exersice 2.21
; =============
(defn square-list-recur [l]
  (let [head (first l)
        other (rest l)]
    (if (not (seq l))
      []
      (cons (* head head) (square-list-recur other))))) 
(defn square-list [l]
  (map #(* % %) l))

; Exercise 2.22
;==============
(defn square-list-iter [items]
  (letfn [(iter [things answer]
            (let [head (first things)]
              (if (not (seq things))
                answer
                (recur (rest things) (cons (* head head) answer)))))]
    (iter items [])))

; This implementation starts with the empty list []
; and cons adds to the the left side and we consume
; also from the source from the left as well.
; 
; The second implementation does not work becase the second
; argument must be a list

; Exercise 2.23
; =============
(defn for-each [f l]
  (let [useless (map f l)]
    (reduce (fn [x y] true) useless)))

(defn for-each-not-so-smart [f l]
  (when (seq l)
    (f (first l))
    (recur f (rest l))))

; Exercise 2.24
; =============
;(1 (2 (3 4)))
; 
;        (1 (2 (3 4)))
;             / \
;            1  (2 (3 4))
;                  / \
;                 2  (3 4)
;                     / \
;                    3   4

; Exercise 2.25
; =============
(def x225 (list 1 3 (list 5 7) 9))
; (first (rest (first (rest (rest x225)))))

(def y225 (list (list 7)))
; (first (first))

(def z225 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(defn next-ele [l] ((comp first rest) l))
; (first (rest (next-ele (next-ele (next-ele (next-ele (next-ele z225)))))))

; Exercise 2.26
;==============
(def x226 (list 1 2 3))
(def y226 (list 4 5 6))
;
;user=> (concat x226 y226)                   
;(1 2 3 4 5 6)
;user=> (cons x226 y226)
;((1 2 3) 4 5 6)
;user=> (list x226 y226)
;((1 2 3) (4 5 6))

; Exercise 2.27
;==============
(defn deep-reverse [l]
  (letfn [(iter [acum l]
            (if (seq l)
              (recur (cons (deep-reverse (first l)) acum) (rest l))
              acum))]
    (if (seq? l)
      (iter [] l)
      l)))

; Exercise 2.28
;==============
(defn fringe [l]
  (letfn [(iter [acum l]
            (cond
              (not (seq? l))  (cons l acum)
              (seq l)         (recur (iter acum (first l)) (rest l))
              :else           acum))]
   (reverse (iter [] l))))

; Exercise 2.29
;==============
(defn make-mobile [left right]
  (list left right))
(defn make-branch [length structure]
  (list length structure))
;1.
(defn left-branch [mobile]
  (first mobile))
(defn right-branch [mobile]
  (first (rest mobile)))

(defn branch-length [branch]
  (first branch))
(defn branch-structure [branch]
  (first (rest branch)))

;2.
(defn weight-branch [branch]
  (let [structure (branch-structure branch)]
    (if (seq? structure)
      (+ (weight-branch (left-branch structure))
         (weight-branch (right-branch structure)))
      structure)))

(defn total-weight [mobile]
  (+ (weight-branch (left-branch mobile))
     (weight-branch (right-branch mobile))))

;3.
(defn balanced-mobile? [mobile]
  (letfn [(balance-branch [branch]
            (* (branch-length branch) (weight-branch branch)))]
    (if (seq? mobile)
      (and (= (balance-branch (left-branch mobile))
              (balance-branch (right-branch mobile)))
           (balanced-mobile? (branch-structure (left-branch mobile)))
           (balanced-mobile? (branch-structure (right-branch mobile))))
      true)))

;4.
; Only the constructor and accesors require changes.

; Exercise 2.30
; =============
(defn square-tree [tree]
  (letfn [(square [n] (* n n))]
    (cond
      (not (seq? tree)) (square tree)
      (empty? tree)     nil
      :else             (cons (square-tree (first tree))
                              (square-tree (rest tree))))))

(defn square-tree-map [tree]
  (map #(if (seq? %) (square-tree-map %) (* % %)) tree))


; Exsercise 2.31
;===============
(defn tree-map [f tree]
  (map #(if (seq? %) (tree-map f %) (f %)) tree))

; Exercise 2.32
; =============
(defn subsets [s]
  (cond
    (empty? s)  [[]]
    :else       (let [others (subsets (rest s))] 
                  (concat others (map #(cons (first s) %) others)))))

; the recursion stops when we empty the initial list
; from there we start rebuilding backward
; s:[]                      -> [[]]
; s:[n] others:[[]]         -> [[] [n]]
; s:[n-1 n] others:[[] [n]] -> [[] [n] [n-1] [n-1 [n]]]
;...
;we cancat the already rebuilt list with itself 
;having been attached the previous element to each of its element

; Exercise 2.33
;==============
(defn accumulate [op initial seq-data]
  (if (empty? seq-data)
    initial
    (op (first seq-data)
        (accumulate op initial (rest seq-data)))))

(defn my-map [p seq-data]
  (accumulate (fn [x y] (cons (p x) y)) [] seq-data))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(defn length [seq1]
  (accumulate (fn [x y] (inc y)) 0 seq1))

; Exercise 2.34
; =============
(defn horner-eval [x coeff-seq]
  (accumulate (fn [this-coeff higher-terms] 
                (+ this-coeff (* x higher-terms))) 0 coeff-seq))

; Exercise 2.35
; =============
;[1 2]
;(cl [1 2])
;(cl

(defn count-leaves [t]
  (letfn [(aux [tree]
            (if (or (vector? tree) (seq? tree) (list? tree)) (count-leaves tree) 1))]
    (accumulate (fn [x y] (+ x y)) 0 (map aux t))))

;(cl [[1 2] 3])
;(acum f 0 [(cl [1 2]) (cl 3)])
;(cl [1 2]) -> (acum f 0 [1 1]) -> 2
;(cl 3) -> 1
;(acum f 0 [2 1])

; Exercise 2.36
; =============
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    []
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

; Exercise 2.37
; =============
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn m-*-v [m v]
  (map (partial dot-product v) m))

(defn transpose [m]
  (accumulate-n cons [] m))

(defn m-*-m [m n]
  (let [cols (transpose n)]
    (map (partial m-*-v cols) m)))

; Exercise 2.38
; =============
(defn fold-left [op initial seq-data]
  (letfn [(iter [result others]
            (if (empty? others)
              result
              (recur (op result (first others)) (rest others))))]
    (iter initial seq-data)))

(def fold-right accumulate)
;
;user=> (fold-left / 1 (list 1 2 3))
;1/6
;user=> (fold-right / 1 (list 1 2 3))        
;3/2
;user=> (fold-left list nil (list 1 2 3))
;(((nil 1) 2) 3)
;user=> (fold-right list nil (list 1 2 3))
;(1 (2 (3 nil)))

; Commutative!!

; Exercise 2.39
; =============
(defn r-reverse [seq-data]
  (fold-right (fn [x y] (concat y [x])) nil seq-data))
(defn l-reverse [seq-data]
  (fold-left conj nil seq-data))


; Exercise 2.40
; =============
(defn flatmap [proc data]
  (accumulate concat nil (map proc data)))

(defn unique-pairs [n]
  (flatmap #(map (partial list %) (range 1 %)) (range 1 (inc n))))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (first (rest pair)))))

(defn make-pair-sum [pair]
 (concat pair (list (+ (first pair) (first (rest pair)))))) 

(defn prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

; Execise 2.41
; ============
(defn ordered-triples [n]
  (flatmap #(map (partial cons %) (unique-pairs (dec %))) (range 1 (inc n))))

(defn triples-that-sum [n s]
  (filter #(= s (apply + %)) (ordered-triples n)))

