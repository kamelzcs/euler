import Control.Monad (replicateM)

primes :: [Int]
primes = 2 : filter (null . tail . primeFactors) [3, 5..]

primeFactors n = factors n primes

factors n (p : ps)
    | p * p > n = [n]
    | n `mod`p == 0 = p : factors (n `div` p) ps
    | otherwise = factors n ps

isPrime :: Int -> Bool
isPrime = null . tail . primeFactors

primeCandidate = [1,3,7,9]

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

rot n l = y ++ x where (x,y) = splitAt n l
allrots l = map (\x -> rot x l) [0..(length l)-1]
isCircular l =  all (isPrime . fromDigits) $ allrots l

circular :: Int -> [[Int]]
circular 1 = [[2],[3],[5],[7]]
circular n = filter isCircular $ replicateM n primeCandidate

solve = length $ concatMap circular [1..6]

main = print $ solve
