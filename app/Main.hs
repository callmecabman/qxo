{-# LANGUAGE TupleSections #-}

module Main where

import Data.Maybe (catMaybes)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type Turn = Int

data Player = Cross
            | Nought
            | Star
            deriving (Show, Eq)

type Move = (Player, Turn)

data Direction = U | D | L | R | UL | UR | DL | DR
  deriving (Show, Eq)

anti :: Direction -> Direction
anti d = case d of
           U  -> D
           D  -> U
           L  -> R
           R  -> L
           UL -> DR
           UR -> DL
           DL -> UR
           DR -> UL

data Link = Topo Direction
          | Quantum Move
          deriving (Show, Eq)

board :: Gr () Link
board = let es = [ (1, 2, Topo R ), (1, 4, Topo D), (1, 5, Topo DR)
                 , (2, 3, Topo R ), (2, 5, Topo D), (2, 4, Topo DL), (2, 6, Topo DR)
                 , (3, 5, Topo DL), (3, 6, Topo D)
                 , (4, 5, Topo R ), (4, 7, Topo D), (4, 8, Topo DR)
                 , (5, 6, Topo R ), (5, 7, Topo DL), (5, 8, Topo D), (5, 9, Topo DR)
                 , (6, 9, Topo D ), (6, 8, Topo DL)
                 , (7, 8, Topo R )
                 , (8, 9, Topo R )
                 ]
         in mkGraphSym (fmap (, ()) [1..9]) es

inv (x, y, l) = case l of
                  Quantum q -> (y, x, Quantum q)
                  Topo d -> (y, x, Topo (anti d))

insEdgeSym e@(x,y,l) g = if x /= y
                            then insEdge (inv e) $ insEdge e g
                            else case l of
                                   Quantum _ -> insEdge e g
                                   Topo _ -> insEdge (inv e) $ insEdge e g

delLEdgeSym e g = delLEdge (inv e) $ delLEdge e g

mkGraphSym ns es = let es' = es ++ fmap inv es
                    in mkGraph ns es'

isClassic :: Graph gr => gr () Link -> Node -> Bool
isClassic g n = let (_, _, _, from) = context g n
                 in n `elem` fmap snd (filter (\x -> case fst x of Topo _ -> False; Quantum _ -> True) from)

pathInDirection :: Graph gr => gr () Link -> Direction -> Int -> Node -> [Node]
pathInDirection g d k n = let (_, _, _, from) = context g n
                              candidate = fmap snd $ filter (\x -> fst x == Topo d) from
                           in if k == 0
                                 then []
                                 else [n] ++ (candidate >>= pathInDirection g d (k - 1))

pathAllDirections :: Graph gr => gr () Link -> Node -> Int -> [[Node]]
pathAllDirections g n k = let p x = pathInDirection g x k n
                           in fmap p [R, UR, U, UL]

nodePlayer :: Graph gr => gr () Link -> Node -> Maybe Player
nodePlayer g n = let (_, _, _, from) = context g n
                     q = filter (\x -> case x of Topo _ -> False; Quantum _ -> True) $ fmap fst $ filter (\x -> snd x == n) from
                  in if null q
                        then Nothing
                        else case q of
                               [Quantum (p, _)] -> Just p

samePlayer :: Graph gr => gr () Link -> [Node] -> Bool
samePlayer _ []     = False
samePlayer _ [n]    = True
samePlayer g (n:ns) = case nodePlayer g n of
                        Nothing -> False
                        p       -> all (== p) $ fmap (nodePlayer g) ns

winner :: Graph gr => gr () Link -> [Player]
winner g = let w x = pathAllDirections g x 3
               straightPaths = filter (\x -> length x == 3) $ concatMap w (nodes g)
               classicPaths  = filter (\x -> all (isClassic g) x) straightPaths
               spPaths       = filter (samePlayer g) classicPaths
            in if null spPaths
                  then []
                  else catMaybes $ fmap ((nodePlayer g) . head) spPaths

newboard = insEdgeSym (8, 8, Quantum (Nought, 2))
         -- $ insEdgeSym (3, 3, Quantum (Cross, 7))
         -- $ insEdgeSym (2, 2, Quantum (Cross, 5))
         -- $ insEdgeSym (1, 1, Quantum (Cross, 3))
         -- $ insEdgeSym (1, 1, Topo DR)
         -- $ delLEdgeSym (1, 5, Topo DR)
         $ insEdge (1, 8, Topo L)
         $ insEdge (8, 1, Topo D)
         $ insEdgeSym (1, 6, Quantum (Cross, 1)) board

main :: IO ()
main = do
  prettyPrint newboard
  print $ isClassic newboard 2
  return ()
