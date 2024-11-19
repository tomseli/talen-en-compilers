module P0 where

-- 1
unwords' :: [String] -> String
unwords' []     = []
unwords' [x]    = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

unwords'' :: [String] -> String 
unwords'' = foldr f []
  -- where f x []  = x 
    where    f x acc = x ++ " " ++ acc    

words' :: String -> [String]
words' [] = [] 
words' xs = takeWhile (/= ' ') xs : words' (drop 1 $ dropWhile (/= ' ') xs)

-- 2
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' _ acc [] = acc
foldl'' f acc (x:xs) = foldl'' f (f acc x) xs

-- 3
digitsToNumber :: [Int] -> Int
digitsToNumber xs = fst $ foldr f (0, 0) xs
  where f x (acc, exp) = (acc + x * 10 ^ exp, exp + 1)
