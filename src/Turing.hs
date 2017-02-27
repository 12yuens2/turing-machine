module TuringMachine where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

import System.IO
import System.Environment

import TuringTape

import Debug.Trace

type State = String

data Move = L | R
	deriving (Show, Eq)

data Cell = Cell String | Blank
    deriving (Show, Eq, Ord)

type TapeAlphabet = (Set.Set Cell)
type TMTape = (Tape Cell)

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

run :: TM -> TMTape -> Maybe TMTape
run tm tp = step tm tp

step :: TM -> TMTape -> Maybe TMTape
step (TM q a qa qr d) tp
    | q `elem` qa = Just tp
    | q `elem` qr = Nothing
    | otherwise   = step (TM newState a qa qr d) newTp
        where
            symbol      = currentCell tp
            tf          = head $ filter (\x -> inState x == q && inSymbol x == symbol) d
            newState    = outState tf
            wroteTape   = writeToCell tp (outSymbol tf)
            newTp       
                | outMove tf == L = tapeLeft wroteTape
                | outMove tf == R = tapeRight wroteTape Blank