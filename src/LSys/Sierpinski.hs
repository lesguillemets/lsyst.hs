module LSys.Sierpinski (sierpinski) where
import LSys.LSys
import Turtle
import Data.Set (fromList)

sierpinski :: LSys Char
sierpinski = LSys {
    _alph = fromList "AB+-",
    _state = "A",
    _rule = sieRule,
    _display = sieDraw
}

sieRule :: Char -> String
sieRule 'A' = "+B-A-B+"
sieRule 'B' = "-A+B+A-"
sieRule c = return c

sieDraw :: [Char] -> IO ()
sieDraw = turtleDraw "Sierpinski" (Turtle (0,0) 0) . map toCommand

toCommand :: Char -> Command
toCommand 'A' = Draw baseLength
toCommand 'B' = Draw baseLength
toCommand '+' = Turn (pi/3)
toCommand '-' = Turn (-pi/3)

baseLength :: Double
baseLength = 2
