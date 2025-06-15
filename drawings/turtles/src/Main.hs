
import Diagrams.Prelude hiding (lerp, connect, end, skyblue)
import Diagrams.Backend.SVG.CmdLine

import Pitaya

import PathUtils (expand)
import DrawingUtils (drawLines, drawAtPoints)

-- Some colors
green1 = sRGB24read "#11665b"
blackishGreen = sRGB24read "#051E1B"
burnt = sRGB24read "#66111c"
skyblue = sRGB24read "#41d9ef"
blackred = sRGB24read "#0A0102"

turtle1Radius = 15
turtle2Radius = 17

-- directions for rendering pulp
render :: Pulp -> Diagram B
render p
  | (edgeid $ fromPath p) == "turtle1" = circle 0.5 # fc green1 # lw 1 # lc (blend 0.2 green1 white) 
  | (edgeid $ fromPath p) == "turtle2" = circle 0.5 # fc green1 # lw 1 # lc (blend 0.1 green1 skyblue)
  | (edgeid $ fromPath p) == "water1" = circle 1 # fc blackishGreen # lw 1 # lc (blend 0.3 green1 burnt)
  | otherwise = mempty

water1 :: Pitaya.Path
water1 = expand "water1" 0.5 30 0.01

turtle1 :: Pitaya.Path
turtle1 = expand "turtle1" 0.1 turtle1Radius 0.05

turtle2 :: Pitaya.Path
turtle2 = expand "turtle2" turtle2Radius 0.01 0.05 # pathMap (\ e -> e # eNd (rvrs $ node e))

connector1 :: Pitaya.Path
connector1 = empte
  # eNd (Node { nRadius = 20 , nTurns = 1 })
  # eRds 18
  # eTrns 0.4

connector2 :: Pitaya.Path
connector2 = empte
  # eNd (Node { nRadius = turtle1Radius - 0.75 , nTurns = 1 }) 
  # eRds 37
  # eTrns 0.875

drawing :: Diagram B
drawing = (drawAtPoints Main.render 
  $ pitaya (
    water1 
    <> connector1 
    <> turtle1
    <> connector2
    <> turtle2
  ))
  <> square 60 # fc blackred # lw none

main = mainWith drawing

