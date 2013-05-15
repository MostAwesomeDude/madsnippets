import Data.List

-- f x y = gcd x y > 1
f x y = x `mod` y == 0

primes = nubBy f [2..]

main = mapM print primes
