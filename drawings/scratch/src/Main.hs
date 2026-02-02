{-# LANGUAGE RecordWildCards #-}
import Diagrams.Prelude hiding (lerp, connect)
import Diagrams.Backend.SVG.CmdLine

import Pitaya
import MathUtils (lerp)
import PathUtils (expand, connect, knot1)
import DrawingUtils (drawAtPoints)

-- Names of paths
rootName = "root"
greyBranchName = "greyBranch"
whiteBranchName = "whiteBranch"

render :: Pulp -> Diagram B
render (Pulp {..}) 
  | (edgeid fromPath) == rootName = circle 0.1 # fc red # lc black # lw 0.5
  | (edgeid fromPath) == greyBranchName = circle 0.1 # fc white # lc grey # lw 0.5
  | (edgeid fromPath) == whiteBranchName = circle 0.1 # fc grey # lc white # lw 0.5
  | otherwise = mempty

rootNode :: Pitaya.Node
rootNode = Node { nRadius = 0.5 , nTurns = 2 }

root :: Pitaya.Path
root = knot1 greyBranchName 0
  <> knot1 greyBranchName 0.25
  <> knot1 whiteBranchName 0


drawingPt1 = drawAtPoints Main.render $ pitaya $
  root

backgroundFill = (square 20 # fc skyblue # lc black)

drawing :: Diagram B
drawing = 
  drawingPt1
  -- Background fill is a white square
  <> backgroundFill

main = mainWith drawing
