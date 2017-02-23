import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

import System.IO
import System.Environment

type State = String

data Move = L | R
	deriving (Show, Eq)

data Cell = Cell String | Blank
    deriving (Show, Eq)

type Tape = Map.Map Int Cell
--type TapeAlphabet = (Set String)
--type InputAlphabet = (Set String)

data TM = TM { currState :: State,
               qa        :: [State],
               qr        :: [State],
               delta     :: [Transition]
             }
    deriving (Show)

data Transition = Transition {  inState    :: State,
                                inSymbol   :: Cell,
                                outState   :: State,
                                outSymbol  :: Cell,
                                outMove    :: Move
                             }
    deriving (Show)

initTapeList :: String -> Int -> [(Int, Cell)]
initTapeList [] _ = []
initTapeList (c:cs) i = (i, Cell [c]):(initTapeList cs (i+1))

initTape :: String -> Tape
initTape s = Map.fromList (initTapeList s 0)

run :: TM -> Tape -> Maybe Tape
run tm tp = step tm 0 tp

step :: TM -> Int -> Tape -> Maybe Tape
step (TM q qa qr d) pos tp
    | q `elem` qa = Just tp
    | q `elem` qr = Nothing
    | otherwise   = step (TM newState qa qr d) newPos newTp
        where
            symbol      = fromMaybe Blank $ Map.lookup pos tp
            tf          = head $ filter (\x -> inState x == q && inSymbol x == symbol) d
            newState    = outState tf
            newTp       = Map.insert pos (outSymbol tf) tp
            newPos
                | pos < 0           = 0
                | outMove tf == L   = pos - 1
                | outMove tf == R   = pos + 1



parseStates :: [String] -> TM
parseStates xs = TM { currState = head xs,
                      qa        = map getCleanState $ filter (\x -> List.isInfixOf "+" x) xs,
                      qr        = map getCleanState $ filter (\x -> List.isInfixOf "-" x) xs,
                      delta     = []
                    }

getCleanState :: String -> String
getCleanState xs = [x | x <- xs, not (x `elem` "+- ")]


parseTransitions :: [String] -> TM -> TM
parseTransitions xs (TM a b c _) 
    = TM { currState = a, qa = b, qr = c,
           delta = map createTransition $ map words xs
         } 

createTransition :: [String] -> Transition
createTransition [iState, iSym, oState, oSym, m] 
    = Transition iState (createCell iSym) oState (createCell oSym) move
    where
        move | m == "L" = L
             | m == "R" = R

createCell :: String -> Cell
createCell s | '_' `elem` s = Blank
             | otherwise    = Cell s

createTM :: [String] -> TM
createTM (x:xs) = parseTransitions ts tm 
    where
        numStates = read $ [last x]
        ts        = drop (numStates+1) xs
        tm        = parseStates $ take numStates xs

main :: IO ()
main = do
    args <- getArgs

    content <- readFile $ args !! 0
    input   <- readFile $ args !! 1

    let tm      = createTM $ lines content
    let tp      = initTape input

    let result  = run tm tp
    putStrLn $ show result
    