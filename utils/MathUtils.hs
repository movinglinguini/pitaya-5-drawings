module MathUtils (lerp, lengthDouble) where

-- Interpolation between two values
lerp :: (Num a) => a -> a -> a -> a
lerp s1 s2 int = s1 + (int * (s2 - s1))

lengthDouble :: [a] -> Double
lengthDouble [] = 0.0
lengthDouble (_ : xs) = 1 + lengthDouble xs