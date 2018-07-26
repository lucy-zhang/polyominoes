module Main where

import Polyominoes

main :: IO ()
main = putStrLn $ show $ map (length . freePolyominoes) [1..5]
