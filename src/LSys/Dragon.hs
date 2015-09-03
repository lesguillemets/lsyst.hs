module LSys.Dragon (dragon) where
import LSys.LSys
import Turtle
import Data.Set (fromList)

dragon :: LSys Char
dragon = LSys {
    _alph = fromList "XYF+-",
    _state = "FX",
    _rule = dragonRule,
    _display = dragonDraw
}

dragonRule :: Char -> String
dragonRule 'X' = "X+YF+"
dragonRule 'Y' = "-FX-Y"
dragonRule c = return c

dragonDraw :: [Char] -> IO ()
dragonDraw = turtleDraw "Dragon" (Turtle (0,0) 0) . map toCommand

toCommand :: Char -> Command
toCommand 'F' = Draw baseLength
toCommand '+' = Turn (pi/2)
toCommand '-' = Turn (-pi/2)
toCommand _ = Stay

baseLength :: Double
baseLength = 2
