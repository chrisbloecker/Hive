module Graph
  where

-------------------------------------------------------------------------------

data Graph = Graph { size :: Int
                   , m    :: [[Int]]
                   }