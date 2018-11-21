-- chapter 5.1

multiThree :: Int -> Int -> Int -> Int
multiThree x y z = x * y * z

-- compareWithHundred :: Int -> Ordering
-- compareWithHundred x = compare 100 x

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

-- chapter 5.2

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--     where g x y = f y x

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

-- chapter 5.3

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' _ [] = []
-- filter' p (x:xs)
--     | p x       = x : filter p xs
--     | otherwise = filter p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1.. 100]))
    where isLong xs = length xs > 15

-- chapter 5.5

sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x : acc) []

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
-- filter' p = foldr (\x acc -> if p x then x : acc else acc) []
filter' p = foldl (\acc x -> if p x then acc ++ [x] else acc) []

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs