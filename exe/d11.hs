module Main (main) where

import D11.Solution (solve)

main :: IO ()
main = getContents >>= solve
