module PathUtils (expand, connect) where

import Pitaya
import MathUtils

expandHelper :: String -> Double -> Double -> Double -> Double -> Pitaya.Path
expandHelper pathName from to int stepSize
  | int >= 1 = Null
  | otherwise = Edge {
    edgeid = pathName
    , node = Node {
      nRadius = lerp from to int
      , nTurns = 1
    }
    , eTurns = 0
    , eRadius = 0
    , next = expandHelper pathName from to (int + stepSize) stepSize
  }

-- Expand into a spiral of increasing size
expand :: String -- Name for the edges in the path
  -> Double -- The starting radius for the spiral
  -> Double -- The ending radius for the spiral
  -> Double -- Step size (should be between 0 and 1)
  -> Pitaya.Path
expand pathName from to stepSize = expandHelper pathName from to 0 stepSize

connectHelper :: String 
  -> Double -- How much the path connecting the nodes turns
  -> Double
  -> Double -- The radius for each edge of the path
  -> [Pitaya.Node] -- The nodes to place in the path
  -> Double -- Interval for the turning
  -> Double -- Step size for the interval
  -> Pitaya.Path
connectHelper _ _ _ _ [] _ _ = Null
connectHelper pathName startTurn endTurn eRadius (n : ns) int stepSize =
  Edge {
    edgeid = pathName
    , node = n
    , eTurns = lerp startTurn endTurn int
    , eRadius = eRadius
    , next = connectHelper pathName startTurn endTurn eRadius ns (int + stepSize) stepSize
  }

connect :: String -- Name for the edges in the path
  -> Double -- How much the path connecting the nodes turns
  -> Double -- 
  -> Double -- The radius for each edge of the path
  -> [Pitaya.Node]
  -> Pitaya.Path
connect pathName startTurn endTurn eRadius ns = 
    connectHelper pathName startTurn endTurn eRadius ns 0 (1 / (lengthDouble ns))
