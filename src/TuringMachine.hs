{- |
Module      :  TuringMachine
Description :  A Turing Machine implementation.
Copyright   :
License     :
Maintainer  :  sy35@st-andrews.ac.uk

Defines all data structures used to create the TuringMachine such as the tape and transition functions. 
-}
module TuringMachine where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe

import TuringTape

import Debug.Trace

type State = String

data Move = L | R
	deriving (Show, Eq)

data Cell = Cell String | Blank
    deriving (Show, Eq, Ord)

type TapeAlphabet = (Set.Set Cell)
type TMTape = (TuringTape Cell)

data TM = TM { currState :: State,
               alphabet  :: TapeAlphabet,
               qa        :: [State],
               qr        :: [State],
               delta     :: [Transition]
             }
    deriving (Show)

data Transition = Transition {  inState    :: State
                              , inSymbol   :: Cell
                              , outState   :: State
                              , outSymbol  :: Cell
                              , outMove    :: Move
                             }
    deriving (Show)

-- | Runs the TuringMachine on the TuringTape.
run :: TM -> TMTape -> Maybe TMTape
run tm tp = step tm tp

step :: TM -> TMTape -> Maybe TMTape
step (TM q a qa qr d) tp
    | q `elem` qa = Just tp
    | q `elem` qr = Nothing
    | otherwise   = trace (show tp) $ trace (show tf) $ step (TM newState a qa qr d) newTp
        where
            symbol      = currentCell tp
            tf          = head $ filter (\x -> inState x == q && inSymbol x == symbol) d
            newState    = outState tf
            wroteTape   = writeToCell tp (outSymbol tf)
            newTp       
                | outMove tf == L = tapeLeft wroteTape
                | outMove tf == R = tapeRight wroteTape Blank