(ns tupelo.math.modular-arithmetic
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    )
  (:import
    [clojure.lang BigInt]))

; #todo move to i
;   tupelo.math.mod.long
;   tupelo.math.mod.BigInteger
;   tupelo.math.mod.BigInt
;
;-----------------------------------------------------------------------------
; #todo need BigInt version?

(defn ceil-long [x] (long (Math/ceil (double x))))
(defn floor-long [x] (long (Math/floor (double x))))
(defn round-long [x] (long (Math/round (double x))))
(defn trunc-long [x] (long (.longValue (double x))))

(s/defn signum :- s/Int
  "Returns either -1, 0, or +1, to indicate the sign of the input. "
  [x :- s/Num]
  (cond
    (pos? x) +1
    (neg? x) -1
    :else 0))

(s/defn same-sign :- s/Bool
  "Returns `true` iff x and y have the same sign, or are both zero."
  [x :- s/Num
   y :- s/Num]
  (truthy?
    (or
      (and (pos? x) (pos? y))
      (and (neg? x) (neg? y))
      (and (zero? x) (zero? y)))))

;-----------------------------------------------------------------------------
; shortcuts for quot/mod with different return types
(s/defn mod-Long :- Long
  "Computes the mod of two Long numbers, returning a Long."
  [n :- Long
   d :- Long] (clojure.core/mod ^Long n ^Long d))

(s/defn quot-Long :- Long
  "Computes the quot of two Long numbers, returning a Long."
  [n :- Long
   d :- Long] (clojure.core/quot ^Long n ^Long d))

(s/defn mod-BigInteger :- BigInteger
  "Computes the mod of two BigInteger numbers, returning a BigInteger."
  [n :- BigInteger
   d :- BigInteger] (.mod ^BigInteger n ^BigInteger d))

(s/defn quot-BigInteger :- BigInteger
  "Computes the quot of two BigInteger numbers, returning a BigInteger."
  [n :- BigInteger
   d :- BigInteger] (.divide ^BigInteger n ^BigInteger d))

(s/defn mod-BigInt :- BigInt
  "Computes the mod of two BigInt numbers, returning a BigInt."
  [n :- s/Int
   d :- s/Int] (mod ^BigInt (bigint n) ^BigInt (bigint d)))

(s/defn quot-BigInt :- BigInt
  "Computes the quot of two BigInt numbers, returning a BigInt."
  [n :- s/Int
   d :- s/Int] (quot ^BigInt (bigint n) ^BigInt (bigint d)))

;-----------------------------------------------------------------------------
;  shortcuts for modular add/mult with different return types
(s/defn add-mod-Long :- Long
  "Adds two numbers a and b (mod N)."
  [a :- Long
   b :- Long
   N :- Long]
  (assert (and (pos? N) (< 1 N)))
  (it-> (+ a b)
    (mod-Long it N)))

(s/defn mult-mod-Long :- Long
  "Multiply two numbers a and b (mod N)."
  [a :- Long
   b :- Long
   N :- Long]
  (assert (and (pos? N) (< 1 N)))
  (it-> (* a b)
    (mod-Long it N)))

(s/defn add-mod-BigInteger :- BigInteger
  "Adds two numbers a and b (mod N)."
  [a :- BigInteger
   b :- BigInteger
   N :- BigInteger]
  (assert (and (pos? N) (< 1 N)))
  (it-> (.add ^BigInteger a ^BigInteger b)
    (mod-BigInteger it N)))

(s/defn mult-mod-BigInteger :- BigInteger
  "Multiply two numbers a and b (mod N)."
  [a :- BigInteger
   b :- BigInteger
   N :- BigInteger]
  (assert (and (pos? N) (< 1 N)))
  (it-> (.multiply ^BigInteger a ^BigInteger b)
    (mod-BigInteger it N)))

(s/defn add-mod-BigInt :- BigInt
  "Adds two numbers a and b (mod N)."
  [a :- s/Int
   b :- s/Int
   N :- s/Int]
  (assert (and (pos? N) (< 1 N)))
  (it-> (+ a b)
    (mod-BigInt it N)))

(s/defn mult-mod-BigInt :- BigInt
  "Multiply two numbers a and b (mod N)."
  [a :- s/Int
   b :- s/Int
   N :- s/Int]
  (assert (and (pos? N) (< 1 N)))
  (it-> (*  a b)
    (mod-BigInt it N)))

;-----------------------------------------------------------------------------
(s/defn mod-symmetric :- s/Int
  "Like clojure.core/mod, but returns a result symmetric around zero [-N/2..N/2). N must be even and positive."
  [i :- s/Int
   N :- s/Int]
  (assert (and (int? i) (int-pos? N) (even? N)))
  (let [d-ovr-2 (/ N 2)
        result  (cond-it-> (mod i N)
                  (<= d-ovr-2 it) (- it N))]
    result))

(s/defn mod-inverse :- s/Int
  "Computes the 'inverse` y of a number x (mod N), such that `x*y (mod N)` = 1.
   Uses the extended Euclid algorithm (iterative version). Assumes x and N are relatively prime. "
  [x :- s/Int
   N :- s/Int]
  (assert (and (pos? x) (pos? N) (< x N)))
  (let [N-orig N
        a      1
        b      0]
    (if (= 1 N)
      (throw (ex-info "Invalid N" (vals->map x N)))
      (loop [x x
             n N
             a a
             b b]
        (if (< 1 x)
          (let [x-next n
                N-next (mod x n)
                q      (quot x n)
                a-next b
                b-next (- a (* q b))]
            (recur x-next N-next a-next b-next))
          (if (neg? a)
            (+ a N-orig)
            a))))))
