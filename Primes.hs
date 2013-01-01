import Data.List

primes = nubBy (\x y -> gcd x y > 1) [2..]

main = mapM print primes
