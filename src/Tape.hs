module TuringTape where

import qualified Data.Set as Set

data TuringTape a = T [a] a [a]
    deriving (Show)

tapeLeft :: Tape a -> Tape a
tapeLeft (T [] x rs)        = T [] x rs
tapeLeft (T (l:ls) x rs)    = T ls l (x:rs)
tapeLeft _                  = error "Left move"

tapeRight :: Tape a -> a -> Tape a
tapeRight (T ls x []) y     = T (x:ls) y []
tapeRight (T ls x (r:rs)) _	= T (x:ls) r rs
tapeRight _ _               = error "Right move"

currentCell :: Tape a -> a
currentCell (T _ x _ ) = x

writeToCell :: Tape a -> a -> Tape a
writeToCell (T ls _ rs) x = T ls x rs

initTape :: Ord a => [a] -> Set.Set a -> Maybe (Tape a)
initTape (c:cs) alphabet 
    | length (filter (\x -> Set.member x alphabet) (c:cs)) == 0 = Nothing
    | otherwise                                                 = Just $ T [] c cs
