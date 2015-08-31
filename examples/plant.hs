import LSys
import LSys.LSys
import LSys.Plant
main = (\p -> _display p (_state p)) . (!! 6) . iterate nextGen $ plant
