{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Turtle (glossDraw, Turtle) where
import Control.Arrow
import GHC.Float (double2Float)
import Graphics.Gloss

window :: String -> Display
window s = InWindow s (500,500) (30,30)

glossDraw :: String -> Turtle -> [Command] -> IO ()
glossDraw s it cmds = display (window s) black $
    Pictures (execCommands it cmds)

data Turtle = Turtle {
            _loc :: (Double, Double),
            _dir :: Double
}

forward :: Turtle -> Double -> (Double,Double)
forward t d = ((*d) . cos &&& (*d) . sin) . _dir $ t
floatise :: (Double,Double) -> (Float,Float)
floatise (x,y) = (double2Float x, double2Float y)
(+:) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(a,b) +: (c,d) = (a+c,b+d)

data Command = Jump Double
             | Draw Double
             | Turn Double

execCommands :: Turtle -> [Command] -> [Picture]
execCommands t [] = []
execCommands t@(Turtle{..}) (c:cs) =
        case c of
            (Jump d) -> execCommands (t{_loc = _loc +: forward t d}) cs
            (Draw d) -> let
                newLoc = _loc +: forward t d in
                    (color white . line . map floatise $ [_loc, newLoc]):
                        execCommands (t{_loc = newLoc}) cs
            (Turn arg) -> execCommands (t{_dir = _dir + arg}) cs
