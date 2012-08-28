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

; Exercise 2.42
; =============
(def empty-board [])

(defn enumerate-interval [a b]
  (range a (inc b)))

(defn adjoin-pos [row col board]
  (cons [col row] board))

;    1  2  3
;  ---------
;1 | x    y
;2 |   x
;3 |

(defn safe? [k board]
  (letfn [(same-row? [[_ row1] [_ row2]]
            (= row1 row2))
          (same-diagonal? [[col1 row1] [col2 row2]]
            (= (Math/abs (- col1 col2))
               (Math/abs (- row1 row2))))]
    (and
      (not-any? (partial same-row? (first board)) (rest board))
      (not-any? (partial same-diagonal? (first board)) (rest board)))))
          
(defn queens [board-size]
  (letfn [(queen-cols [k]
    (if (zero? k)
      [empty-board]
      (filter 
        #(safe? k %)
        (flatmap 
          #(map (fn [new-row] (adjoin-pos new-row k %))
                (enumerate-interval 1 board-size))
          (queen-cols (dec k))))))]
  (queen-cols board-size)))

;    1 2 3 4 5 6 7 8
;  -------------------
;1 |*  
;2 |             * 
;3 |         * 
;4 |               *
;5 |  * 
;6 |       *
;7 |           *  
;8 |    *

; Exercise 2.43
;==============
(defn queens-slow [board-size]
  (letfn [(queen-cols [k]
    (if (zero? k)
      [empty-board]
      (filter 
        #(safe? k %)
        (flatmap 
          #(map (fn [new-row] (adjoin-pos new-row k %))
                ;(enumerate-interval 1 board-size))
                (queen-cols (dec k)))
          (enumerate-interval 1 board-size)))))]
  (queen-cols board-size)))

; ?????


; Exercise 2.44
;==============
;(defn up-split [painter n]
;  (if (zero? n)
;    painter
;    (let [smaller (up-split (dec n))]
;      (below painter (beside smaller smaller)))))
;
;
;; Exercise 2.45
;;==============
;(defn split [combine with]
;  (letfn [(split-aux [painter n]
;            (if (zero? 0)
;              painter
;              (let [smaller (split-aux (dec n))]
;                (conbine painter (with smaller smalle)))))]
;    (fn [painter n] (split-aux painter n))))
;
;(def right-split-de (split beside below))
;(def up-split-ds (split below beside))
;

; Exercise 2.46
;==============
(defn make-vect [x y]
  [x y])
(defn xcor-vect [vect]
  (first vect))
(defn ycor-vect [vect]
  (last vect))

(defn add-vect [v w]
  [(+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))])
(defn sub-vect [v w]
  [(- (xcor-vect v) (xcor-vect w))
   (- (ycor-vect v) (ycor-vect w))])
(defn scale-vect [factor v]
  [(* factor (xcor-vect v))
   (* factor (ycor-vect v))])

; Exercise 2.47
; =============
(defn make-frame [origin edge1 edge2]
  [origin edge1 edge2])

(defn origin-frame [frame]
  (first frame))
(defn edge1-frame [frame]
  (second frame))
(defn edge2-frame [frame]
  (last frame))

(defn make-frame2 [origin edge1 edge2]
  (cons origin [[edge1 edge2]]))
(defn origin-frame2 [frame]
  (first frame))
(defn edge1-frame2 [frame]
  (first (last frame)))
(defn edge2-frame2 [frame]
  (second (last frame)))

; Exercise 2.48
; =============
(defn make-segment [o-vect e-vect]
 [o-vect e-vect]) 
(defn start-segment [segment]
  (first segment))
(defn end-segment [segment]
  (last segment))

; Exercise 2.49
;==============
(defn frame-coord-map [frame]
  (fn [v]
    (add-vect 
         (origin-frame frame)
         (add-vect (scale-vect (xcor-vect v)
                               (edge1-frame frame))
                   (scale-vect (ycor-vect v)
                               (edge2-frame frame))))))

(defn draw-line [origin segment]
  (println "Line from" origin "with vector " segment))

(defn segments->painter [segment-list]
  (fn [frame]
    (for-each
      (fn [segment]
        (draw-line 
              ((frame-coord-map frame) (start-segment segment))
              ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(def left-side    (make-segment (make-vect 0 0) (make-vect 0 1)))
(def top-side     (make-segment (make-vect 0 1) (make-vect 1 1)))
(def right-side   (make-segment (make-vect 1 1) (make-vect 1 0)))
(def bottom-side  (make-segment (make-vect 1 0) (make-vect 0 0)))
(def outline-painter (segments->painter [left-side top-side right-side bottom-side])) 

(def top-left (make-segment (make-vect 0 1) (make-vect 1 0)))
(def bottom-left (make-segment (make-vect 0 0) (make-vect 1 1)))
(def cross-painter (segments->painter [top-left bottom-left]))

(def mid-left-top (make-segment (make-vect 0 0.5) (make-vect 0.5 1)))
(def mid-top-right (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))
(def mid-right-bottom (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))
(def mid-bottom-left (make-segment (make-vect 0.5 0) (make-vect 0 0.5))) 
(def diamon-painter (segment->painter [mid-left-top mid-top-right, mid-right-bottom, mid-buttom-left]))

; Exercise 2.50
; =============
(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (let [new-origin (m origin)]
        (painter
          (make-frame new-origin 
                (sub-vect (m corner1) new-origin)
                (sub-vect (m corner2) new-origin)))))))

(defn flip-vert [painter]
 (transform-painter painter
           (make-vect 0.0 1.0)
           (make-vect 1.0 1.0)
           (make-vect 0.0 0.0)))

(defn flip-horiz [painter]
  (transform-painter painter
           (make-vect 1.0 0)
           (make-vect 0.0 0.0)
           (make-vect 1.0 1.0)))
(defn rotate180 [painter]
  (transform-painter painter
           (make-vect 1.0 1.0)
           (make-vect 0.0 1.0)
           (make-vect 1.0 0.0)))  
(defn rotate270 [painter]
  (transform-painter painter
           (make-vect 0.0 1.0)
           (make-vect 0.0 0.0)
           (make-vect 1.0 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
           (make-vect 1.0 0.0)
           (make-vect 1.0 1.0)
           (make-vect 0.000.0)))

; Exercise 2.51
;==============
(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)]
    (let [paint-left (tranform-painter painter1
                                       (make-vect 0.0 0.0)
                                       split-point
                                       (make-vect 0.0 1.0))
          paint-right (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.5 1.0))]
     (fn [frame]
      (paint-left frame)
      (paint-right frame))))) 

(defn below [painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)]
    (let [paint-top (transform-painter painter1
                                       split-point
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0))
          paint-bottom (transform-painter painter2
                                          (make-vect 0.0 0.0)
                                          (make-vect 1.0 0.0)
                                          split-point)]
      (fn [frame]
        (paint-top frame)
        (paint-bottom frame)))))

; But the images is rotated.....
(defn below2 [painter1 painter2]
  (let [side-by-side (beside painter1 painter2)]
    (rotate90 side-by-side)))

; Exercise 2.52
; =============
; TODO
