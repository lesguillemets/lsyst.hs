module LSys.Plant (plant) where

import LSys.LSys
import Turtle
import Data.Set (fromList)

plant :: LSys Char
plant = LSys {
    _alph = fromList "XF+-[]",
    _state = "X",
    _rule = plantRule,
    _display = plantDraw
}

plantRule :: Char -> [Char]
plantRule 'X' = "F-[[X]+X]+F[+FX]-X"
plantRule 'F' = "FF"
plantRule  c = return c

plantDraw = turtleSimlate "Tree" (Turtle (0,0) 0) [] . map toCommand

toCommand :: Char -> (Turtle -> Stack -> (Command,Stack))
toCommand 'X' = \ _ s -> (Stay, s)
toCommand 'F' = \ _ s  -> (Draw drawLength, s)
toCommand '[' = \ t s -> (Stay, (_loc t, _dir t):s)
toCommand ']' = \ _ s -> (SetState (head s), tail s)
toCommand '+' = \ _ s -> (Turn (pi/7), s)
toCommand '-' = \ _ s -> (Turn (-pi/7), s)

type Stack = [((Double,Double),Double)]

drawLength :: Double
drawLength = 5
