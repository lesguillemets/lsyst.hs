module LSys.PythagorasTree (pythagoras) where
import LSys.LSys
import Turtle
import qualified Data.Set as S
import Graphics.Gloss

pythagoras :: LSys Char
pythagoras = LSys {
    _alph = S.fromList "01[]",
    _state = "0",
    _rule = pyrule,
    _display = pyDraw
}

pyrule :: Char -> String
pyrule '1' = "11"
pyrule '0' = "1[0]0"
pyrule c = return c

pyDraw :: [Char] -> IO()
pyDraw = turtleSimlate "Pyt" (Turtle (0,0) 0) [] . concatMap toCommands

-- TODO: Better implementation
toCommands :: Char -> [Turtle -> Stack -> (Command, Stack)]
toCommands '0' = return $ \ _ s -> (Draw leafLength, s)
toCommands '1' = return $ \ _ s -> (Draw baseLength, s)
toCommands '[' = return $ \t s -> (Turn (4/pi), (_loc t,_dir t):s)
toCommands ']' = [
                 \t s -> (SetState (head s), tail s),
                 \_ s -> (Turn (-4/pi), s)
                 ]
-- toCommands '1' = \ _ s return $ (Draw baseLength, s))
-- toCommands '[' = \ t s return $ (Turn (4/pi), (_loc t:s)))
-- toCommands ']' = \ t s [(Turn (-4/pi), s), (JumpTo (head s), tail s)])

type Stack = [((Double,Double), Double)]
leafLength :: Double
leafLength = 10
baseLength :: Double
baseLength = 20
