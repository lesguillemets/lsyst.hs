import LSys
import LSys.LSys
import LSys.Koch
main = (\p -> _display p (_state p)) . (!! 5) . iterate nextGen $ koch

