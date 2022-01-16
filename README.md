# primes

Some clojure function about prime numbers, without overflow checks for speed.

## Usage

Add :deps alias in deps.edn

```clojure
{:deps {caesarhu.math/primes {:git/url "https://github.com/caesarhu/primes"
                              :sha "04602f15faab114ea7f80c485c410af276ce228a"}}}
```

## Example usage

```clojure
(require '[caesarhu.primes :as p])

primes: primes lazy sequence
(take 10 p/primes)
=> (2 3 5 7 11 13 17 19 23 29)

(p/is-prime? 17)
=> true

(p/primes-range 17)
=> (2 3 5 7 11 13 17)

(p/primes-range 11 97)
=> (11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)

(p/factors 100)
=> (5 5 2 2)

(factors 134046 (p/primes-range 677))
=> (677 11 3 3 2)

(p/next-probable-prime 468486496846768746873)
=> 468486496846768746901

(p/probable-prime? 468486496846768746873)
=> false

(p/probable-prime? 468486496846768746901)
=> true

(p/divisors 100)
=> (1 2 4 5 10 20 25 50 100)

(p/count-divisors 100)
=> 9
```

## Run test
```clojure
clojure -T:build test
```

## License

Copyright © 2021 caesarhu

Distributed under the Eclipse Public License version 1.0.
