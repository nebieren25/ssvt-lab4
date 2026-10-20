-- Time Spent: 50 min

module Exercise1 where

import Data.List (intersect, nub)
import LTS
import Test.QuickCheck


-- States set Q must be non-empty
prop_statesNonEmpty :: IOLTS -> Bool
prop_statesNonEmpty (states, _, _, _, _) = not (null states)

-- Initial state q0 must belong to states Q
prop_q0InStates :: IOLTS -> Bool
prop_q0InStates (states, _, _, _, q0) = q0 `elem` states

-- States list Q must not contain duplicates
prop_uniqueStates :: IOLTS -> Bool
prop_uniqueStates (states, _, _, _, _) = length states == length (nub states)

-- Input labels LI and Output labels LU must be strictly disjoint
prop_disjointLabels :: IOLTS -> Bool    
prop_disjointLabels (_, inputs, outputs, _, _) = null (inputs `intersect` outputs)

-- Neither LI nor LU should contain reserved labels ("tau", "delta")
prop_noReservedLabels :: IOLTS -> Bool
prop_noReservedLabels (_, inputs, outputs, _, _) =
    let reserved = ["tau", "delta"]
    in null (inputs `intersect` reserved) && null (outputs `intersect` reserved)

-- Input labels should start with '?' and output labels with '!'
prop_labelPrefixes :: IOLTS -> Bool
prop_labelPrefixes (_, inputs, outputs, _, _) =
    all (\i -> case i of (c:_) -> c == '?'; [] -> False) inputs &&
    all (\o -> case o of (c:_) -> c == '!'; [] -> False) outputs

-- Input and Output label lists must not contain duplicate labels
prop_uniqueLabels :: IOLTS -> Bool
prop_uniqueLabels (_, inputs, outputs, _, _) =
    length inputs == length (nub inputs) && length outputs == length (nub outputs)

-- All transitions must connect states present in Q
prop_validTransitionStates :: IOLTS -> Bool
prop_validTransitionStates (states, _, _, transitions, _) =
    all (\(from, _, to) -> from `elem` states && to `elem` states) transitions

-- All transition labels must belong to LI, LU, or be "tau"
prop_validTransitionLabels :: IOLTS -> Bool
prop_validTransitionLabels (_, inputs, outputs, transitions, _) =
    let validLabels = "tau" : (inputs ++ outputs)
    in all (\(_, lbl, _) -> lbl `elem` validLabels) transitions

-- Transition relation T must not contain duplicate transitions
prop_uniqueTransitions :: IOLTS -> Bool
prop_uniqueTransitions (_, _, _, transitions, _) =
    length transitions == length (nub transitions)


validateLTS :: IOLTS -> Bool
validateLTS lts =
    prop_statesNonEmpty lts &&
    prop_q0InStates lts &&
    prop_uniqueStates lts &&
    prop_disjointLabels lts &&
    prop_noReservedLabels lts &&
    prop_labelPrefixes lts &&
    prop_uniqueLabels lts &&
    prop_validTransitionStates lts &&
    prop_validTransitionLabels lts &&
    prop_uniqueTransitions lts