module Main where
import Data.List
import Data.Ord
import Pyramid
import Control.Monad
import Parser
    
main = do
    putStrLn "Podaj nazwe pliku z piramida:"
    fname <- getLine
    content <- readFile fname
    print "Wczytana zawartosc"
    print content
    let temp = parse word content
    let temp2 = parse terms (unconsumed temp)
    let hights = value temp2
    let n = (length hights) `div` 4
    let up = decodeHights (take n hights)
    let down = decodeHights (take n (drop (n*1) hights))
    let left = decodeHights (take n (drop (n*2) hights))
    let right = decodeHights (take n (drop (n*3) hights))
    let pyramid = Pyramid up down left right (Board (newBoard n n n))
    print "Stan poczatkowy piramidy"
    print pyramid
    solvePyramid (applyConstraints pyramid) 100


                      
