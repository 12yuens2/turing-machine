module TuringMachineParser where

import Data.List.Split

import TuringMachine


parseAlphabet :: [String] -> Maybe TapeAlphabet
parseAlphabet (x:y:xs)
  | x /= "alphabet" && length xs /= read y = Nothing
  | otherwise                              = Just s
      where 
        set = Set.empty
        s   = foldr Set.insert set $ map createCell xs 

parseStates :: Maybe TapeAlphabet -> [String] -> Maybe TM
parseStates set xs
    | isNothing set = Nothing
    | otherwise     = Just TM { currState = head xs,
                                alphabet  = fromJust set,
                                qa        = map getCleanState $ filter (\x -> List.isInfixOf "+" x) xs,
                                qr        = map getCleanState $ filter (\x -> List.isInfixOf "-" x) xs,
                                delta     = []
                              }

getCleanState :: String -> String
getCleanState xs = [x | x <- xs, not (x `elem` "+- ")]


parseTransitions :: [String] -> TM -> TM
parseTransitions xs (TM cState a aState rState _) 
    = TM { currState = cState, alphabet = a, qa = aState, qr = rState,  
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

createTM :: [String] -> Maybe TM
createTM (x:xs)
    | isNothing tm = Nothing
    | otherwise    = Just $ parseTransitions ts $ fromJust tm 
        where
            numStates = read $ last $ words x
            s         = parseAlphabet $ splitOn " " $ head $ drop numStates xs
            ts        = drop (numStates+1) xs
            tm        = parseStates s $ take numStates xs