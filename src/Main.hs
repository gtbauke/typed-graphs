module Main (main) where

import Graphs

graph :: Graph Int
graph = Graph [1 --> 2, 1 --> 3, 2 --> 3, 3 --> 4, 4 --> 1]

main :: IO ()
main = do
  print graph
  putStrLn ""
  print $ vertices graph
  putStrLn ""
  print $ adjacent graph (Vertex 1)
