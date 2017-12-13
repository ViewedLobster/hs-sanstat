module Sanstat where

import Data.List

mean :: Floating a => [a] -> a
mean xs = sum xs / genericLength xs

var :: Floating a => [a] -> a
var xs = 
    sum [ (x - m)**2 | x <- xs ] / (genericLength xs - 1)
    where m = mean xs

std :: Floating a => [a] -> a
std = sqrt . var

-- Confidence intervals (normal with alpha = 0.05)
nfint :: Floating a => [a] -> (a, a)
nfint xs =
    (m - d*1.96, m + d*1.96)
    where m = mean xs; d = sqrt (mean xs / genericLength xs)

nfConfInt :: Floating a => a -> a -> (a,a)
nfConfInt mu d = (mu - 1.96*d, mu + 1.96*d)

confInt :: Floating a => a -> a -> a -> (a, a)
confInt mu d fact = (mu - fact*d, mu + fact*d)

-- poisson probability
pop :: Floating a => a -> Integer -> a
pop mu k = mu**(fromInteger k) / (factorial k) * exp (-mu)

factorial :: Floating a => Integer -> a
factorial k = fromInteger (product [1..k])

-- binomial probability
binoprob :: Fractional a  => Int -> a -> [Int] -> a
binoprob n p (x:xs) = fromIntegral (n `choose` x) * p^^x * (1 - p)^^(n - x) + binoprob n p xs
binoprob n p [] = 0

-- n choose k
instance Num a => Num[a] where
    fromInteger n = [fromInteger n]
    (x:xs) + (y:ys) = (x + y) : (xs + ys)
    xs + [] = xs
    [] + xs = xs
    (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
    _ * _ = []

choose :: Int -> Int -> Int
choose n k = ([1,1]^n) !! k

-- Linear regression, one variable
meanx :: Floating a => [(a,a)] -> a
meanx = mean . fmap fst
meany :: Floating a => [(a,a)] -> a
meany = mean . fmap snd 

sxy :: Floating a => [(a,a)] -> a
sxx :: Floating a => [(a,a)] -> a
syy :: Floating a => [(a,a)] -> a
sxydiff :: Floating a => [(a,a)] -> a -> a -> a
sxxdiff :: Floating a => [(a,a)] -> a -> a
syydiff :: Floating a => [(a,a)] -> a -> a

sxy xs = sxydiff xs (meanx xs) (meany xs)
sxx xs = sxxdiff xs (meanx xs)
syy xs = syydiff xs (meany xs)

sxydiff (x:xs) dx dy = (fst x - dx)*(snd x - dy) + sxydiff xs dx dy
sxydiff [] dx dy = 0

sxxdiff (x:xs) dx = (fst x - dx)**2 + sxxdiff xs dx 
sxxdiff [] dx = 0

syydiff (x:xs) dy = (snd x - dy)**2 + syydiff xs dy 
syydiff [] dx = 0

betastar xs = sxy xs / sxx xs
alphastar xs = meany xs - (betastar xs)*meanx xs

s xs = 
    sqrt ((1/ (genericLength xs - 2)) * sum [ snd t - alps - bets*fst t | t <- xs])
    where   alps = alphastar xs
            bets = betastar xs

ssq xs = 1 / (genericLength xs - 2) * ( syy xs - betastar xs * sxy xs)
    
-- chi-squared tests
qval1 :: Floating a => [a] -> [a] -> a -> a
qval1 (x:xs) (y:ys) n = (x - n*y)**2 / (n*y) + qval1 xs ys n
qval1 [] [] n = 0

qval2 :: Floating a => [[a]] -> [a] -> a
qval2 xss plist = sum (map (\x -> qval1 x plist (sum x)) xss)

plistcalc :: Floating a => [[a]] -> [a]

plistcalc xss = let n = sum (map sum xss)
                    l = length (xss !! 1) in
    map (/n) (foldl (zipWith (+)) (replicate l 0) xss)

