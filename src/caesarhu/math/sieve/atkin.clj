(ns caesarhu.math.sieve.atkin
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.reducers :as r]
            [injest.path :refer [+> +>> x>> =>>]]))

(def base 60)
(def origin-primes [false false true true false true])
(def s1 #{1,13,17,29,37,41,49,53})
(def s2 #{7,19,31,43})
(def s3 #{11,23,47,59})
(def s-all (clojure.set/union s1 s2 s3))

(defn set-prime
  [a n bool]
  (swap! a assoc n bool))

(defn toggle-prime
  [a n]
  (swap! a update-in [n] not))

(defn unset-square
  [a]
  (let [limit (count @a)
        sqrt (math/sqrt limit)]
    (doseq [r (range 5 sqrt)
            :when (get @a r)
            :let [r2 (* r r)]]
      (doseq [i (range r2 limit r2)]
        (set-prime a i false)))))

(defn filter-primes
  [v]
  (for [i (range 2 (count v))
        :when (get v i)]
    i))

(defn atkin-v1
  [^long limit]
  (let [sqrt (inc (first (math/exact-integer-sqrt limit)))
        primes (atom (vec (concat origin-primes (repeat (- limit 5) false))))]
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
          (toggle-prime primes q1))
        (when (and (< q2 limit) (s2 (mod q2 base)))
          (toggle-prime primes q2))
        (when (and (< y x) (< 0 q3 limit) (s3 (mod q3 base)))
          (toggle-prime primes q3))))
    (unset-square primes)
    (filter-primes @primes)))

(defn atkin-v2
  [^long limit]
  (let [sqrt (math/sqrt limit)
        primes (atom (vec (concat origin-primes (repeat (- limit 5) false))))
        squares (vec (map #(* % %) (range sqrt)))
        q1-q2 (fn [a s]
                (=>> (drop 1 squares)
                     (map #(* a %))
                     (take-while #(< % limit))
                     (mapcat (fn [xx]
                               (take-while #(< % limit) (map #(+ xx %) (drop 1 squares)))))
                     (filter #(s (mod % base)))
                     (map #(toggle-prime primes %))))]
    (=>> (range 2 sqrt)
         (take-while #(< (+ (* 2 (squares %)) (* 2 %) -1) limit))
         (mapcat (fn [x]
                   (=>> (range (dec x) 0 -2)
                        (map #(squares %))
                        (map #(- (* 3 x x) %))
                        (take-while #(< % limit))
                        (filter #(s3 (mod % base))))))
         (map #(toggle-prime primes %)))
    (q1-q2 4 s1)
    (q1-q2 3 s2)
    (unset-square primes)
    (filter-primes @primes)))

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

(defn atkin-v3
  [^long limit]
  (let [sqrt (first (math/exact-integer-sqrt limit))]
    (loop [x 1
           vx (vec (concat origin-primes (repeat (- limit 5) false)))]
      (if (> x sqrt)
        (->> vx remove-square filter-primes)
        (let [xx (* x x)
              xx3 (* 3 xx)
              vy (loop [y 1
                        v vx]
                   (if (> y sqrt)
                     v
                     (let [yy (* y y)]
                       (recur (inc y) (cond-> v
                                        (when-let [q (and (odd? y) (+ xx3 xx yy))]
                                          (and (< q limit) (s1 (mod q base)))) (toggle (+ xx3 xx yy))
                                        (when-let [q (and (odd? x) (even? y) (+ xx3 yy))]
                                          (and (< q limit) (s2 (mod q base)))) (toggle (+ xx3 yy))
                                        (when-let [q (and (< y x) (odd? (+ x y)) (- xx3 yy))]
                                          (and (< q limit) (s3 (mod q base)))) (toggle (- xx3 yy)))))))]
          (recur (inc x) vy))))))

(comment
  (atkin-v3 100)
  (time (count (atkin-v3 1000000)))
  (time (count (atkin-v2 1000000))))