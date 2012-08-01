import Data.List

primes = nubBy (\x y -> gcd x y > 1) [2..]

main = putStr $ show primes
