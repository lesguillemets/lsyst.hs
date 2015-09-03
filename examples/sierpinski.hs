import LSys
import LSys.LSys
import LSys.Sierpinski
main = (\p -> _display p (_state p)) . (!! 8) . iterate nextGen $ sierpinski
