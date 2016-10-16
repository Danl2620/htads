module Util where

import qualified Data.Char as Char

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

wrap :: Int -> String -> String
wrap width fullStr =
    unlines $ map wrapStr (lines fullStr)
    where wrapStr str = if length str <= width
                        then str
                        else let chop = words $ take width str
                                 len = length chop
                                 pre = (unwords $ take (len - 1) chop) in
                             pre ++ "\n" ++ (wrap width $ dropWhile Char.isSpace $ drop (length pre) str)

