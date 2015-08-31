{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Turtle (turtleDraw, turtleSimlate, Turtle(..), Command(..)) where
import Control.Arrow
import GHC.Float (double2Float)
import Graphics.Gloss

window :: String -> Display
window s = InWindow s (500,500) (30,30)

turtleDraw :: String -> Turtle -> [Command] -> IO ()
turtleDraw s it cmds = display (window s) black $
    Pictures (execCommands it cmds)

turtleSimlate :: String -- title
              -> Turtle -> s -- initial turtle and initial state
              -> [Turtle -> s -> (Command, s)] -- commands
              -> IO ()
turtleSimlate title initTurtle initState cmds = display (window title) black $
    Pictures (simTurtle initTurtle initState cmds)

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
             | JumpTo (Double, Double)
             | DrawTo (Double, Double)
             | SetAng Double
             | SetState ((Double, Double), Double)
             | Stay

-- TODO :: Better abstraction
execCommands :: Turtle -> [Command] -> [Picture]
execCommands initTurtle = simTurtle initTurtle undefined . map (
        \c -> (\_ _ -> (c, undefined)))

-- TODO : Rewrite using State Monad
simTurtle :: Turtle -> s -> [Turtle -> s -> (Command, s)] -> [Picture]
simTurtle _ _ [] = []
simTurtle !t@(Turtle{..}) s (cFunc:cs) = let (c, nextState) = cFunc t s in
    case c of
        (Jump d) -> simTurtle (t{_loc = _loc +: forward t d}) nextState cs
        (Draw d) -> let
            newLoc = _loc +: forward t d in
                (color white . line . map floatise $ [_loc, newLoc]):
                    simTurtle (t{_loc = newLoc}) nextState cs
        (Turn arg) -> simTurtle (t{_dir = _dir + arg}) nextState cs
        (JumpTo d) -> simTurtle (t{_loc = d}) nextState cs
        (DrawTo d) -> (color white . line . map floatise $ [_loc,d]):
                            simTurtle (t{_loc=d}) nextState cs
        (SetAng ang) -> simTurtle (t{_dir = ang}) nextState cs
        (SetState (loc,ang)) -> simTurtle (t{_dir=ang, _loc=loc}) nextState cs
        Stay -> simTurtle t nextState cs
