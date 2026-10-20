-- Time Spent: 45 min

module Exercise2 where

import Data.List (nub)
import LTS
import Test.QuickCheck
import Exercise1



-- Generator for strictly valid IOLTS structures
genValidIOLTS :: Gen IOLTS
genValidIOLTS = do
    -- Non-empty state set Q = [1..numStates] and q0
    numStates <- choose (1, 10) :: Gen Integer
    let states = [1..numStates]
        q0 = 1

    -- 2. Disjoint pools for input and output labels
    inputPool  <- sublistOf ["?a", "?b", "?c", "?d"]
    outputPool <- sublistOf ["!x", "!y", "!z", "!w"]

    -- 3. Generate transitions T using valid states and available labels
    let availableLabels = "tau" : (inputPool ++ outputPool)
    numTransitions <- choose (0, numStates * 3)
    rawTransitions <- vectorOf (fromIntegral numTransitions) $ do
        from <- elements states
        to <- elements states
        lbl <- elements availableLabels
        return (from, lbl, to)

    let transitions = nub rawTransitions

    return (states, inputPool, outputPool, transitions, q0)

-- | Generator for intentionally invalid IOLTS structures (for negative testing)
genInvalidIOLTS :: Gen IOLTS
genInvalidIOLTS = do
    validLTS <- genValidIOLTS
    mutationType <- choose (1, 4) :: Gen Int
    case mutationType of
        -- Invalid initial state (q0 not in Q)
        1 -> let (states, inputs, outputs, trans, _) = validLTS
             in return (states, inputs, outputs, trans, 999)

        -- Overlapping label sets (LI intersect LU /= [])
        2 -> let (states, inputs, outputs, trans, q0) = validLTS
             in return (states, "?shared" : inputs, "?shared" : outputs, trans, q0)

        -- Empty state set (Q = [])
        3 -> let (_, inputs, outputs, trans, q0) = validLTS
             in return ([], inputs, outputs, trans, q0)

        -- Transition referencing non-existent state
        _ -> let (states, inputs, outputs, trans, q0) = validLTS
                 badTrans = (999, "tau", 1) : trans
             in return (states, inputs, outputs, badTrans, q0)



prop_validIOLTS :: Property
prop_validIOLTS = forAll genValidIOLTS validateLTS

prop_invalidIOLTS :: Property
prop_invalidIOLTS = forAll genInvalidIOLTS (not . validateLTS)


main :: IO ()
main = do
    putStrLn "=== 1. Testing Valid IOLTS Generator ==="
    quickCheck prop_validIOLTS

    putStrLn "\n=== 2. Testing Invalid IOLTS Generator ==="
    quickCheck prop_invalidIOLTS