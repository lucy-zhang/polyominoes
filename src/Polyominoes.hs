module Polyominoes where

import Data.Dequeue
import Data.List
import Data.List.Split
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set

type Block = (Int, Int)

newtype Polyomino = Polyomino [Block]
    deriving (Eq, Show)

type Neighbors = BankersDequeue Block

type Visited = Set Block

data SearchState = SearchState Polyomino Neighbors Visited
    deriving Show

polyominoes :: Int -> [Polyomino]
polyominoes n = [ poly | SearchState poly _ _ <- iterate step [start] !! n ]
    where start = SearchState (Polyomino []) (fromList [(0, 0)]) (Set.singleton (0, 0))
          step = concat . map next

next :: SearchState -> [SearchState]
next s@(SearchState (Polyomino bs) neighbors visited) = map newState $ allHeadTails neighbors
    where
        newState (b, n) = let
                poly' = Polyomino (b : bs)
                neighbors' = foldl pushBack n $ newNeighbors b
                visited' = foldr Set.insert visited $ newNeighbors b
            in SearchState poly' neighbors' visited'
        newNeighbors (x, y) = let candidates = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]
            in [ b | b <- candidates, b > (0, 0) && Set.notMember b visited ]

allHeadTails :: (Dequeue q) => q a -> [(a, q a)]
allHeadTails q = case popFront q of
    Nothing -> []
    Just x@(_, q') -> x : (allHeadTails q')

translate :: (Int, Int) -> Polyomino -> Polyomino
translate (i, j) (Polyomino bs) = Polyomino $ map (\(x, y) -> (x + i, y + j)) bs

variants :: Polyomino -> [Polyomino]
variants p = [ align . rot . refl $ p | rot <- fnPowers 4 rotate, refl <- fnPowers 2 reflect ]
    where fnPowers n f = take n $ iterate (f .) id
          reflect (Polyomino bs) = Polyomino $ map (\(x, y) -> (-x, y)) bs
          rotate (Polyomino bs) = Polyomino $ map (\(x, y) -> (y, -x)) bs
          align p@(Polyomino bs) = translate (-i, -j) p where (i, j) = minimum bs

equivalent :: Polyomino -> Polyomino -> Bool
equivalent (Polyomino bs1) p2 = any (== sort bs1) [ sort bs | Polyomino bs <- variants p2 ]

freePolyominoes :: Int -> [Polyomino]
freePolyominoes n = nubBy equivalent $ polyominoes n

prettyFormat :: Polyomino -> String
prettyFormat p = unlines $ map (map (\b -> if b then '#' else ' ')) (matrix p)

matrix :: Polyomino -> [[Bool]]
matrix p@(Polyomino bs) = let
    minY = fst $ minimum $ map swap bs
    Polyomino bs' = translate (0, -minY) p
    n = length bs
    in chunksOf n $ map (\b -> elem b bs') [ (i, j) | i <- [0..n-1], j <- [0..n-1] ]

printPolyominoes :: [Polyomino] -> IO ()
printPolyominoes = mapM_ putStrLn . map prettyFormat
