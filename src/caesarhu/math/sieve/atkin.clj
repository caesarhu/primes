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

(defn filter-primes
  [v]
  (=>> (range 2 (count v))
       (filter #(get v %))))

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
          (swap! primes toggle q1))
        (when (and (< q2 limit) (s2 (mod q2 base)))
          (swap! primes toggle q2))
        (when (and (< y x) (< 0 q3 limit) (s3 (mod q3 base)))
          (swap! primes toggle q3))))
    (=>> @primes
         (remove-square)
         (filter-primes))))

(defn atkin-v2
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

(defn atkin-v3
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

(comment
  (atkin-v1 100)
  (time (count (atkin-v3 1000000)))
  (time (count (atkin-v1 1000000))))