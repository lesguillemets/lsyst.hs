import LSys
import LSys.LSys
import LSys.Dragon
main = (\p -> _display p (_state p)) . (!! 14) . iterate nextGen $ dragon
