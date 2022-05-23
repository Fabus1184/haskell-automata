module Misc where

import Regex (Regex)

takeFirstEqual :: (Eq a) => [a] -> a
takeFirstEqual [] = undefined
takeFirstEqual (x : xs)
  | x == head xs = x
  | otherwise = takeFirstEqual xs

printMatrix :: [[Regex]] -> IO ()
printMatrix = mapM_ (putStrLn . (++) "\n" . concatMap ((++ "\t") . show))
