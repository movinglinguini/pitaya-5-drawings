{-# LANGUAGE RecordWildCards #-}
import Diagrams.Prelude hiding (lerp, connect)
import Diagrams.Backend.SVG.CmdLine

import Pitaya
import MathUtils (lerp)
import PathUtils (expand, connect)
import DrawingUtils (drawAtPoints)

moonRadius = 10

deepIndigo = sRGB24read "#051230"
vandykeBrown = sRGB24read "#4b3317"
rawSienna = sRGB24read "#bb7125"

render :: Pulp -> Diagram B
render (Pulp {..})
  | (edgeid fromPath) == "moon" = circle 0.2 # fc deepIndigo # lw 1 # lc (blend 0.8 deepIndigo white)
  | (edgeid fromPath) == "branch" = circle 0.15 # fc rawSienna # lw 2 # lc (darken 0.1 rawSienna)

moon :: Pitaya.Path
moon = expand "moon" 0.1 moonRadius 0.05

node1 :: Pitaya.Node
node1 = Node {
  nRadius = 0.5
  , nTurns = 3
}

connector :: Pitaya.Path
connector = Edge {
  edgeid = "moon"
  , node = Node {
    nRadius = moonRadius
    , nTurns = 0
  }
  , eRadius = 10
  , eTurns = 0
  , next = Null
}

path1 = connect "branch" 0 0.65 3 (take 5 $ repeat node1)
path2 = connect "branch" 0 (-0.65) 3 (take 5 $ repeat node1)
path3 = connect "branch" (-0.1) (-0.75) 3 (take 5 $ repeat node1)
path4 = connect "branch" (-0.3) (-0.75) 3.5 (take 5 $ repeat node1)
path5 = connect "branch" (-0.3) (-0.5) 4 (take 3 $ repeat node1)

drawing :: Diagram B
drawing = (drawAtPoints Main.render $ pitaya 
  (
    moon
    <> connector
    <> path1
    <> path2
    <> path3
    <> path4
    <> path5
  ))
  <> (square 30 # fc vandykeBrown # lc (darken 0.4 vandykeBrown))

main = mainWith drawing
