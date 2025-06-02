module DrawingUtils (drawDots, drawAtPoints) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine


import Pitaya

-- Draw a diagram at each point designated by the pulp
drawAtPoints :: (Pulp -> Diagram B) -> [Pulp] -> Diagram B
drawAtPoints _ [] = mempty
drawAtPoints f (p : ps) =  drawAtPoints f ps <> atPoints [ point p ] (repeat $ f p)

-- Draw dots of designated radius and color at each point designated by the pulp.
drawDots :: (Pulp -> (Double, Colour Double)) -> [Pulp] -> Diagram B
drawDots f ps = drawAtPoints (\ p -> (circle (fst $ f p) # fc (snd $ f p)) ) ps

-- atPoints [ point p ] (repeat $ circle (fst $ f p) # fc (snd $ f p) # lw none) 
--                         <> drawDots f ps
