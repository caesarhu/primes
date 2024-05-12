(ns caesarhu.math.primes
  (:require [clojure.math.numeric-tower :as math]
            [caesarhu.math.sieve.rosetta :refer [primes-paged]]
            [clojure.core.reducers :as r]))

(set! *unchecked-math* true)

(defn primes
  "Lazy sequence of prime numbers"
  ([]
   (let [p (primes-paged)]
     (lazy-seq
      (cons (first p)
            (next p)))))
  ([^long limit]
   (take-while #(< % limit) (primes)))
  ([^long start ^long limit]
   (->> (primes)
        (drop-while #(< % start))
        (take-while #(< % limit)))))

(defn- test-prime
  "Determine if a number is prime by looping through divisors"
  [^long x]
  (loop [iter 5 top (Math/sqrt x)]
    (cond
      (> iter top) true
      (or (zero? (mod x iter))
          (zero? (mod x (+ 2 iter)))) false
      :else (recur (+ 6 iter) top))))

(defn is-prime?
  "Determines if a given integer is prime."
  [^long x]
  (cond
    (<= x 3) (< 1 x)
    (or (zero? (mod x 2))
        (zero? (mod x 3))) false
    :else (test-prime x)))

(def ^:private DEFAULT_PRIME_CERTAINTY 100)

(defn probable-prime?
  "bigint Miller–Rabin primality test, default certainty 100."
  ([n certainty]
   (.isProbablePrime (biginteger n) certainty))
  ([n]
   (probable-prime? n DEFAULT_PRIME_CERTAINTY)))

(defn next-probable-prime
  "bigint Miller–Rabin primality test to find next probable prime."
  [n]
  (.nextProbablePrime (biginteger n)))

(defn factors
  "find all prime factors of n"
  ([^long n custom-primes]
   (loop [n n
          prime-seq custom-primes
          result []]
     (let [p (first prime-seq)]
       (cond
         (> (* p p) n) (cons n result)
         (zero? (rem n p)) (recur (quot n p) prime-seq (cons p result))
         :else (recur n (next prime-seq) result)))))
  ([^long n]
   (factors n (primes))))

(defn product-coll
  [c1 c2]
  (cond
    (empty? c1) c2
    (empty? c2) c1
    :else (for [x c1 y c2]
            (*' x y))))

(defn divisors
  "Find all divisors of n"
  [^long n]
  (let [power-seq (fn [n power]
                    (for [i (range (inc power))]
                      (math/expt n i)))]
    (reduce product-coll (map #(apply power-seq %) (frequencies (factors n))))))

(defn count-divisors
  "Count all divisors of n"
  [^long n]
  (->> (factors n)
       frequencies
       vals
       (map inc)
       (reduce *)))

(defn totient
  "Find totient function of n"
  [^long n]
  (if (= n 1) 1
      (loop [ps (take-while #(<= (* % %) n) (primes))
             n n
             phi n]
        (if-let [p (first ps)]
          (if (zero? (mod n p))
            (recur (rest ps)
                   (loop [n n]
                     (if (pos-int? (mod n p)) n
                         (recur (quot n p))))
                   (- phi (quot phi p)))
            (recur (rest ps) n phi))
          (if (> n 1)
            (- phi (quot phi n))
            phi)))))

(defn- prime-factor
  [^long limit, ^clojure.lang.PersistentVector n-vec, ^long prime]
  (let [merge-prime (fn [^clojure.lang.PersistentVector n-vec, ^long i, ^clojure.lang.PersistentArrayMap m]
                      (update n-vec i (partial merge-with +) m))]
    (loop [powers (take-while #(< % limit) (iterate (partial * prime) prime))
           n-vec (merge-prime n-vec prime {prime 1})]
      (if (empty? powers) n-vec
          (let [[idx next-idx] (take 2 powers)
                v (reduce (fn [n-vec i]
                            (reduce (fn [n-vec j]
                                      (merge-prime n-vec (+ j i) {prime ((n-vec j) prime)}))
                                    n-vec
                                    (range prime (min (- limit i) (inc idx)) prime)))
                          n-vec
                          (range idx (if next-idx next-idx limit) idx))]
            (recur (rest powers) (if next-idx (merge-prime v next-idx {prime 1}) v)))))))

(defn factors-range
  "Generate all factors to range, return a vector of factros."
  [^long limit]
  (reduce (fn [n-vec prime]
            (if (empty? (n-vec prime))
              (prime-factor limit n-vec prime)
              n-vec))
          (vec (repeat limit {}))
          (range 2 limit)))

(set! *unchecked-math* false)
