(ns caesarhu.math.sieve.atkin
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.reducers :as r]
            [injest.path :refer [+> +>> x>> =>>]])
  (:import [java.util BitSet]))

(set! *unchecked-math* true)

(def base 60)
(def origin-primes [false false true true false true])
(def s1 #{1,13,17,29,37,41,49,53})
(def s2 #{7,19,31,43})
(def s3 #{11,23,47,59})
(def s-all (clojure.set/union s1 s2 s3))

(defn toggle
  [^clojure.lang.PersistentVector v ^long n]
  (update-in v [n] not))

(defn remove-square
  [^clojure.lang.PersistentVector v]
  (let [limit (count v)
        sqrt (math/sqrt limit)]
    (r/reduce (fn [v2 i]
                (if-let [ii (and (get v2 i) (* i i))]
                  (r/reduce #(assoc %1 %2 false)
                            v2 (range ii limit ii))
                  v2))
              v (range 5 (inc sqrt) 2))))

(defn remove-square-bit
  [^long limit ^BitSet v]
  (let [sqrt (first (math/exact-integer-sqrt limit))]
    (doseq [i (range 5 (inc sqrt) 2)
            :when (.get v i)
            :let [ii (* i i)]]
      (doseq [j (range ii limit ii)]
        (.clear v j)))
    v))

(defn filter-primes
  [v]
  (=>> (range 2 (count v))
       (filter #(get v %))))

(defn init-primes
  [^long limit]
  (let [v (BitSet. (inc limit))]
    (.set v 2)
    (.set v 3)
    (.set v 5)
    v))

(defn atkin-v1
  [^long limit]
  (let [sqrt (inc (first (math/exact-integer-sqrt limit)))
        primes (init-primes limit)]
    (doseq [x (range 1 sqrt)
            :let [xx (* x x)
                  xx3 (* 3 xx)
                  xx4 (* 4 xx)]]
      (doseq [y (range 1 sqrt)
              :let [yy (* y y)
                    q1 (+ xx4 yy)
                    q2 (+ xx3 yy)
                    q3 (- xx3 yy)]]
        (when (and (< q1 limit) (s1 (mod q1 base)))
          (.flip primes q1))
        (when (and (< q2 limit) (s2 (mod q2 base)))
          (.flip primes q2))
        (when (and (< y x) (< 0 q3 limit) (s3 (mod q3 base)))
          (.flip primes q3))))
    (remove-square-bit limit primes)
    (take-while pos? (iterate #(.nextSetBit primes (inc %)) 2))))

(defn atkin-v2
  [^long limit]
  (let [sqrt (first (math/exact-integer-sqrt limit))]
    (loop [x 1
           vx (vec (concat origin-primes (repeat (- limit 5) false)))]
      (if (> x sqrt)
        (->> vx remove-square filter-primes)
        (let [xx (* x x)
              xx3 (* 3 xx)
              vy (r/reduce (fn [v y]
                             (let [yy (* y y)]
                               (cond-> v
                                 (when-let [q (and (odd? y) (+ xx3 xx yy))]
                                   (and (< q limit) (s1 (mod q base)))) (toggle (+ xx3 xx yy))
                                 (when-let [q (and (odd? x) (even? y) (+ xx3 yy))]
                                   (and (< q limit) (s2 (mod q base)))) (toggle (+ xx3 yy))
                                 (when-let [q (and (< y x) (odd? (+ x y)) (- xx3 yy))]
                                   (and (< q limit) (s3 (mod q base)))) (toggle (- xx3 yy)))))
                           vx (range 1 (inc sqrt)))]
          (recur (inc x) vy))))))

(defn atkin-v3
  [^long limit]
  (let [sqrt (first (math/exact-integer-sqrt limit))
        primes (BitSet. (inc limit))]
    (.set primes 2)
    (.set primes 3)
    (.set primes 5)
    (doseq [x (range 1 (inc sqrt))
            :let [xx4 (* 4 x x)]
            :while (< xx4 limit)]
      (doseq [y (range 1 (inc sqrt) 2)
              :let [yy (* y y)
                    q (+ xx4 yy)]
              :while (< q limit)
              :when (s1 (mod q base))]
        (.flip primes q)))
    (doseq [x (range 1 (inc sqrt) 2)
            :let [xx3 (* 3 x x)]
            :while (< xx3 limit)]
      (doseq [y (range 2 (inc sqrt) 2)
              :let [yy (* y y)
                    q (+ xx3 yy)]
              :while (< q limit)
              :when (s2 (mod q base))]
        (.flip primes q)))
    (doseq [x (range 2 (inc sqrt))
            :let [xx3 (* 3 x x)]]
      (doseq [y (range (dec x) 0 -2)
              :let [yy (* y y)
                    q (- xx3 yy)]
              :while (< q limit)
              :when (s3 (mod q base))]
        (.flip primes q)))
    (remove-square-bit limit primes)
    (take-while pos? (iterate #(.nextSetBit primes (inc %)) 2))))

(defn atkin-v4
  [^long limit]
  (let [sqrt (first (math/exact-integer-sqrt limit))
        primes (BitSet. (inc limit))]
    (.set primes 2)
    (.set primes 3)
    (.set primes 5)
    (loop [x 1]
      (if (> x sqrt)
        (do
          (remove-square-bit limit primes)
          (take-while pos? (iterate #(.nextSetBit primes (inc %)) 2)))
        (let [xx (* x x)
              xx3 (* 3 xx)]
          (loop [y 1]
            (when (<= y sqrt)
              (let [yy (* y y)]
                (when-let [q (and (odd? y) (+ xx3 xx yy))]
                  (and (< q limit) (s1 (mod q base)) (.flip primes q)))
                (when-let [q (and (odd? x) (even? y) (+ xx3 yy))]
                  (and (< q limit) (s2 (mod q base)) (.flip primes q)))
                (when-let [q (and (< y x) (odd? (+ x y)) (- xx3 yy))]
                  (and (< q limit) (s3 (mod q base)) (.flip primes q)))
                (recur (inc y)))))
          (recur (inc x)))))))

(set! *unchecked-math* false)

(comment
  (atkin-v4 100)
  (time (r/reduce + (atkin-v4 2000000)))
  (time (count (atkin-v3 2000000))))