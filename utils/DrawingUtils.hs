module DrawingUtils (
  drawDots
  , drawAtPoints
  , drawLines
  , drawTrail
) where

import Diagrams.Prelude
import Diagrams.Trail
import Diagrams.Backend.SVG.CmdLine


import Pitaya

-- Draw a diagram at each point designated by the pulp
drawAtPoints :: (Pulp -> Diagram B) -> [Pulp] -> Diagram B
drawAtPoints _ [] = mempty
drawAtPoints f (p : ps) = drawAtPoints f ps <> atPoints [ point p ] (repeat $ f p)

-- Draw dots of designated radius and color at each point designated by the pulp.
drawDots :: (Pulp -> (Double, Colour Double)) -> [Pulp] -> Diagram B
drawDots f ps = drawAtPoints (\ p -> (circle (fst $ f p) # fc (snd $ f p)) ) ps

-- Draw lines
drawLines :: [Pulp] -> Diagram B
drawLines [] = mempty
drawLines [ p ] = mempty
drawLines (p1 : (p2 : ps)) = drawLines ps <> fromVertices [point p1, point p2]

-- Draw a continuous trail
drawTrail :: [Pulp] -> Diagram B
drawTrail ps = fromVertices (pulpsToPoints ps)