{- |
Module      :  TuringTape
Description :  Module that implements a Tape for a Turing Machine as a ListZipper.
Copyright   :
License     :
Maintainer  :  sy35@st-andrews.ac.uk

The Tape for a TuringMachine is implemented as a ListZipper. This gives us O(1) operations on the list.

The cursor of the ListZipper is the current cell being read by the TuringMachine. 
The TuringMachine can then move the cursor left or right and write to the current cell.
-}

module TuringTape where

import qualified Data.Set as Set

data TuringTape a = T [a] a [a]
    deriving (Show)

tapeLeft :: TuringTape a -> TuringTape a
tapeLeft (T [] x rs)        = T [] x rs
tapeLeft (T (l:ls) x rs)    = T ls l (x:rs)
tapeLeft _                  = error "Left move"

tapeRight :: TuringTape a -> a -> TuringTape a
tapeRight (T ls x []) y     = T (x:ls) y []
tapeRight (T ls x (r:rs)) _	= T (x:ls) r rs
tapeRight _ _               = error "Right move"

currentCell :: TuringTape a -> a
currentCell (T _ x _ ) = x

writeToCell :: TuringTape a -> a -> TuringTape a
writeToCell (T ls _ rs) x = T ls x rs

initTape :: Ord a => [a] -> Set.Set a -> Maybe (TuringTape a)
initTape (c:cs) alphabet 
    | length (filter (\x -> Set.member x alphabet) (c:cs)) == 0 = Nothing
    | otherwise                                                 = Just $ T [] c cs
