(ns caesarhu.math.primes-test
  (:require [clojure.test :refer :all]
            [caesarhu.math.primes :refer [is-prime? primes probable-prime? next-probable-prime factors]]))

(deftest primes-test
  (testing "primes functions test."
    (let [primes-count (->> (iterate inc 2)
                            (take 1000)
                            (filter is-prime?)
                            count)]
      (is (= 168 primes-count))
      (is (= primes-count (->> (take-while #(<= % 1001) primes) count)))
      (is (= primes-count (->> (iterate inc 2)
                               (take 1000)
                               (filter probable-prime?)
                               count)))
      (is (= primes-count (->> (iterate next-probable-prime 2)
                               (take-while #(<= % 1001))
                               count))))))

(deftest factors-test
  (testing "factors function test."
    (doseq [_ (range 10)
            :let [bound 1000000000
                  n (rand-int bound)
                  fs (factors n)]]
      (is (= true (every? is-prime? fs)))
      (is (= n (apply * fs))))))
