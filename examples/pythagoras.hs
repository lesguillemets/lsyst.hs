import LSys
import LSys.LSys
import LSys.PythagorasTree
main = (\p -> _display p (_state p)) . (!! 4) . iterate nextGen $ pythagoras

