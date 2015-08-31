module LSys.Koch (koch) where

import LSys.LSys
import Turtle
import Data.Set (fromList)

koch :: LSys Char
koch = LSys {
    _alph = fromList "F+-",
    _state = "F",
    _rule = kochRule,
    _display = kochDraw
}

kochRule :: Char -> String
kochRule 'F' = "F+F-F-F+F"
kochRule c = return c

kochDraw = turtleDraw "koch" (Turtle (0,0) 0) . map toCommand

toCommand :: Char -> Command
toCommand 'F' = Draw baseLength
toCommand '+' = Turn (pi/2)
toCommand '-' = Turn (-pi/2)

baseLength = 5
