module Support.Misc (replace, joinToString, groupEvery, count) where
	
-- import Data.Either

replace :: String -> Char -> Char -> String
replace string a b = [if x == a then b else x | x <- string]

joinToString :: (Show a) => [a] -> Char -> String
joinToString list delimiter = replace (unwords [show x | x <- list]) ' ' delimiter

groupEvery :: Int -> [a] -> [[a]]
groupEvery n list
	| n >= (length list)    = [list]
	| otherwise             = [fst slist] ++ (groupEvery n $ snd slist)
	where slist = splitAt n list

count :: [Bool] -> Int
count [] = 0
count (x:xs) = if x then 1 + (count xs) else (count xs)
