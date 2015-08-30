module LSys.PythagorasTree (pythagoras) where
import LSys.LSys
import qualified Data.Set as S

pythagoras :: LSys Char
pythagoras = LSys {
    _alph = S.fromList "01[]",
    _state = "0",
    _rule = pyrule,
    _display = undefined
}

pyrule :: Char -> String
pyrule '1' = "11"
pyrule '0' = "1[0]0"
pyrule c = return c
