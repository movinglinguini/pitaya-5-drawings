import Diagrams.Prelude hiding (lerp, connect, end, skyblue)
import Diagrams.Backend.SVG.CmdLine

import Pitaya
-- Importing drawAtPoints, which literally draws
-- whatever is specified by `render` at a point.
import DrawingUtils (drawAtPoints)
import PathUtils (expand, connect)

-- Colors
myred = sRGB24read "#7f0010"
darkerred = sRGB24read "#070000"

-- Rendering function. Given a pulp (a point in space), draw something.
render :: Pulp -> Diagram B
render p
  | otherwise = circle 0.5 # fc red # lc black # lw 3

plainAssCircle :: Pitaya.Path
plainAssCircle = expand "" 1 20 0.1 
  <> (empte 
      # eNd Node { nRadius = 20, nTurns = 0.25 }
      # eTrns 0.3
      # eRds 25)

cable :: Pitaya.Path
cable = (empte # eNd (Node { nRadius = 1, nTurns = 0.5 }) # eRds 10 # eTrns 0)

alternatingSpirals :: Pitaya.Path
alternatingSpirals = 
  (empte # eNd (Node { nRadius = 1, nTurns = 3 }) # eRds 10 # eTrns 0.6)
  <> (empte # eNd (Node { nRadius = 1, nTurns = 1 }) # eRds 12 # eTrns 0.6)
  <> (empte # eNd (Node { nRadius = 1, nTurns = 1.75 }) # eRds 12 # eTrns 0.75)
  <> (empte # eNd (Node { nRadius = 1, nTurns = 1 }) # eRds 13 # eTrns 0.8)
  <> (empte # eNd (Node { nRadius = 1, nTurns = 1.75 }) # eRds 12 # eTrns 0.9)
  <> (empte # eNd (Node { nRadius = 1, nTurns = 1 }) # eRds 13 # eTrns 0.95)
  <> cable
  <> cable
  <> (empte # eNd (Node { nRadius = 1, nTurns = 0.5 }) # eRds 10 # eTrns 0.1)
  <> (empte # eNd (Node { nRadius = 1, nTurns = 2 }) # eRds 12 # eTrns 0.25)
  <> cable

drawing :: Diagram B
drawing = (drawAtPoints Main.render 
  $ pitaya (
    plainAssCircle
    <> alternatingSpirals
  )) 
  <> square 80 # fc black # lw none

main = mainWith drawing
