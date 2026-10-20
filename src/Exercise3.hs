-- Time Spent: 110 min

module Exercise3 where

import Data.List (nub)
import LTS
import Test.QuickCheck
import Exercise1 hiding (main)
import Exercise2 hiding (main)


isQuiescent :: IOLTS -> State -> Bool
isQuiescent (_, _, outputs, transitions, _) state =
    null [ l | (s, l, _) <- transitions, s == state, l `elem` outputs || l == "tau" ]

-- Computes suspension traces of an IOLTS
stracesBounded :: Int -> IOLTS -> [Trace]
stracesBounded maxDepth lts@(states, _, _, transitions, q0) =
    nub $ collectTraces maxDepth q0
  where
    deltaTrans = [ (s, "delta", s) | s <- states, isQuiescent lts s ]
    allTrans   = transitions ++ deltaTrans

    collectTraces 0 _ = [[]]
    collectTraces depth currState =
        [] : [ if lbl == "tau" then rest else lbl : rest
             | (s, lbl, nextState) <- allTrans
             , s == currState
             , rest <- collectTraces (depth - 1) nextState
             ]

straces :: IOLTS -> [Trace]
straces = stracesBounded 4

genStrace :: IOLTS -> Gen Trace
genStrace lts = elements (straces lts)


-- Empty trace [] is always in straces
prop_emptyTraceInStraces :: Property
prop_emptyTraceInStraces = forAll genValidIOLTS $ \lts ->
    [] `elem` straces lts

-- Every prefix of a valid suspension trace is also in straces
prop_prefixClosure :: Property
prop_prefixClosure = forAll genValidIOLTS $ \lts ->
    forAll (genStrace lts) $ \tr ->
        all (`elem` straces lts) [ take i tr | i <- [0..length tr] ]

-- All labels in a trace belong to LI, LU, or "delta"
prop_validLabelsInStraces :: Property
prop_validLabelsInStraces = forAll genValidIOLTS $ \lts@(_, inputs, outputs, _, _) ->
    let allowedLabels = "delta" : (inputs ++ outputs)
    in forAll (genStrace lts) $ \tr ->
        all (`elem` allowedLabels) tr

main :: IO ()
main = do
    putStrLn "1. Empty trace presence:"
    quickCheck prop_emptyTraceInStraces

    putStrLn "2. Prefix closure:"
    quickCheck prop_prefixClosure

    putStrLn "3. Label validity:"
    quickCheck prop_validLabelsInStraces