module Sanstat where

import Data.List

mean :: Fractional a => [a] -> a
mean xs = (sum xs) / (genericLength xs)

var :: Fractional a => [a] -> a
var xs = 
	(sum [ (x - m)*(x - m) | x <- xs ]) / genericLength xs
	where m = mean xs

std :: Floating a => [a] -> a
std = sqrt . var

nfint :: Floating a => [a] -> (a, a)

nfint xs =
	(m - d*1.96, m + d*1.96)
	where m = mean xs; d = sqrt (mean xs / genericLength xs)


qval1 :: Floating a => [a] -> [a] -> a -> a
qval1 (x:xs) (y:ys) n = (x - n*y)**2 / (n*y) + qval1 xs ys n
qval1 [] [] n = 0

qval2 :: Floating a => [[a]] -> [a] -> a
qval2 xss plist = sum (map (\x -> qval1 x plist (sum x)) xss)

plistcalc :: Floating a => [[a]] -> [a]

plistcalc xss = let n = sum (map sum xss)
                    l = length (xss !! 1) in
    map (/n) (foldl (zipWith (+)) (replicate l 0) xss)

pop :: Floating a => a -> Integer -> a
pop mu k = mu**(fromInteger k) / (factorial k) * exp (-mu)

factorial :: Floating a => Integer -> a
factorial k = fromInteger (product [1..k])
