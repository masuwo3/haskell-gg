import Data.List
import Data.Char
import qualified Data.Map as Map

-- 6.1

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- 6.2

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` heystack = any (needle `isPrefixOf`) (tails heystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

-- 6.3

-- phoneBook = 
--     [("betty", "555-2983"),
--      ("bonnie", "455-2983"),
--      ("patsy", "355-2983")]

-- findKey :: (Eq k) => k -> [(k,v)] -> v
-- findKey key xs = snd . head . filter (\(k, v) -> k == key) $ xs

-- findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k,v):xs)
--     | key == k  = Just v
--     | otherwise = findKey key xs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key xs = foldr
                    (\(k, v) acc -> if key == k then Just v else acc)
                    Nothing xs

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2983"),
     ("bonnie", "455-2983"),
     ("patsy", "355-2983")]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

-- 6.4