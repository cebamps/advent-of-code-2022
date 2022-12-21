module Main (main) where

import D20.Solution (solve)

main :: IO ()
main = getContents >>= solve
