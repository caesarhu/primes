(ns caesarhu.math.sieve.eratosthenes
  (:require [clojure.core.reducers :as r])
  (:import java.util.PriorityQueue))

(defn bitset-sieve
  [^long bound]
  (let [candidates (java.util.BitSet. bound)]
    ;; initialize array to contain [0 0 2 3 4 5 ...]
    (.set candidates 2 bound)
    ;; eliminate known non-primes
    (loop [idx 2]
      (when (< idx bound)
        (when-let [next-candidate (.get candidates idx)]
          (loop [update-idx (* idx idx)]
            (when (< update-idx bound)
              (.clear candidates update-idx)
              (recur (+ idx update-idx)))))
        (recur (inc idx))))
    ;; return a Clojure seq with only primes in it
    (take-while pos? (iterate #(.nextSetBit candidates (inc %)) 2))))

(let [insert-prime (fn [table x xs]
                     (assoc table (* x x) [(map #(* x %) xs)]))]
  (defn sieve-sm
    ([[i & is]] (sieve-sm is (insert-prime (sorted-map) i is)))
    ([[x & xs] table]
     (let [[next-composite factors] (first table)]
       (if (> next-composite x) ;; x is prime
         (cons x (lazy-seq (sieve-sm xs (insert-prime table x xs))))
         (sieve-sm xs (reduce (fn [table [next-comp & future-comps]]
                                (update table next-comp
                                        conj future-comps))
                              (dissoc table next-composite)
                              factors)))))))

(let [insert-prime (fn [^PriorityQueue table x xs]
                     (.add table [(* x x) (map #(* x %) xs)])
                     table)]
  (defn sieve-pq
    ([[i & is]] (sieve-pq
                 is
                 (insert-prime
                  (PriorityQueue. 10 (fn [[x] [y]] (< x y)))
                  i is)))
    ([[x & xs] ^PriorityQueue table]
     (let [[next-composite] (.peek table)]
       (if (> next-composite x)
         (cons x (lazy-seq (sieve-pq xs (insert-prime table x xs))))
         (do (while (== x (first (.peek table)))
               (let [[_ [f & fs]] (.poll table)]
                 (.add table [f fs])))
             (sieve-pq xs table)))))))

(def wheel2357
  (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8
          6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10]))

(defn spin
  [[x & xs] n]
  (cons n (lazy-seq (spin xs (+ n x)))))

(defn spin-primes
  [sieve-fn]
  (concat [2 3 5 7 11]
          (sieve-fn (spin wheel2357 11))))

(defn bitset-sieve2
  [^long bound]
  (let [candidates (java.util.BitSet. bound)]
    ;; initialize array to contain [0 0 2 3 4 5 ...]
    (.set candidates 2)
    (doseq [idx (range 3 bound 2)]
      (.set candidates idx))
    ;; eliminate known non-primes
    (loop [idx 3]
      (when (< idx bound)
        (when-let [next-candidate (.get candidates idx)]
          (loop [update-idx (* idx idx)]
            (when (< update-idx bound)
              (.clear candidates update-idx)
              (recur (+ idx update-idx)))))
        (recur (+ idx 2))))
    ;; return a Clojure seq with only primes in it
    (take-while pos? (iterate #(.nextSetBit candidates (inc %)) 2))))

(comment
  (time (->> (take-while #(< % 10000000) (spin-primes sieve-pq))
             count))
  (time (count (take-while #(< % 2000000) (spin-primes sieve-sm))))
  (time (count (bitset-sieve2 100000000)))
  (time (count (bitset-sieve 100000000)))
  )
