(ns predicates)

(defn sum-f [f g x]
  (+ (f x)
     (g x)))

(defn less-than [n]
  (fn [x] (< x n) ))

(defn equal-to [n]
  (fn [x] (== x n)))

(defn set->predicate [a-set]
  (fn [x]
    (reduce #(or %1 %2) false 
            (map #(= % x) a-set))))


(defn pred-and [pred1 pred2]
  (fn [x]
    (and (pred1 x)
         (pred2 x))))

(defn pred-or [pred1 pred2]
  #(or (pred1 %) (pred2 %)))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (boolean (award (:awards book))))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (fn [x] (has-award? book x)) awards))

(defn my-some [pred a-seq]
 (cond (empty? a-seq) false
       :else (or (pred (first a-seq)) (my-some pred (rest a-seq)))))

(defn my-every? [pred a-seq]
  (cond (empty? a-seq) true
        :else (and (pred (first a-seq)) (my-every? pred (rest a-seq)))))

(defn prime? [n]
  (let [divides? #(= 0 (mod n %))]
    (not (some divides? (range 2 n)))))
;^^
