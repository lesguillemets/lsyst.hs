module LSys.LSys where
import qualified Data.Set as S
import Data.Set (Set)

data LSys a = LSys {
          _alph :: Set a,
          _state :: [a],
          _rule :: a -> [a],
          _display :: [a] -> IO ()
}

instance Show a => Show (LSys a) where
    show = show . _state

nextGen :: LSys a -> LSys a
nextGen s = s {_state = concatMap (_rule s) (_state s)}
