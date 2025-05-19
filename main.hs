main :: IO ()
main = print (primesBelow 100)

primesBelow :: Int -> [Int]
primesBelow limit = primeStep 3 limit [2]


primeStep x limit foundPrimes
    | x > limit = foundPrimes
    | checkPrime x foundPrimes = primeStep (x+1) limit (foundPrimes ++ [x])
    | otherwise = primeStep (x+1) limit foundPrimes

checkPrime :: Int -> [Int] -> Bool
checkPrime x [] = True
checkPrime x (p : primes) 
    | p * p > x = True 
    | mod x p == 0 = False 
    | otherwise = checkPrime x primes