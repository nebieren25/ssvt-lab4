
module Exercise4 where
import LTS
import Exercise3 (straces)
import Exercise2 (genValidIOLTS)
import Test.QuickCheck


-- Find states reachable with one visible action
nextStates :: IOLTS -> [State] -> Label -> [State]
nextStates (_, _, _, transitions, _) states action =
  makeSet
    [ target | (source, label, target) <- transitions , source `elem` states, label == action]

-- Find all states reachable through zero or more tau steps
tauClosure :: IOLTS -> [State] -> [State]
tauClosure (_, _, _, transitions, _) initial =
  go (makeSet initial)
  where
    go states =
      let reachable =
            makeSet
              (states ++ [ target | (source, label, target) <- transitions, source `elem` states, label == tau])
      in if reachable == states
         then states
         else go reachable

-- Check whether a state is quiescent
isQuiescent :: IOLTS -> State -> Bool
isQuiescent (_, _, outputs, transitions, _) state =
  not (any isActive transitions)
  where
    isActive (source, label, _) =  source == state && (label == tau || label `elem` outputs)

--- Follow the trace, including tau closure and delta observations
after :: IOLTS -> Trace -> [State]
after system@(_, inputs, outputs, _, initial) trace =
  foldl step (tauClosure system [initial]) trace
  where
    step states action
      | action == delta = filter (isQuiescent system) states
      | action `elem` (inputs ++ outputs) = tauClosure system (nextStates system states action)
      | otherwise = []

-- Example models for concrete tests
exampleModel :: IOLTS
exampleModel = createIOLTS [(0, "?coin", 1), (1, tau, 2), (2, "!coffee", 3)]

branchingModel :: IOLTS
branchingModel = createIOLTS [(0, "?a", 1), (0, "?a", 2), (1, "!x", 3), (2, "?b", 4)]

concreteTests :: [(String, Bool)]
concreteTests =
  [ ("Empty trace", after exampleModel [] == [0])
  , ("Tau closure", after exampleModel ["coin"] == [1,2])
  , ("Input and output", after exampleModel ["coin","coffee"] == [3])
  , ("Invalid trace", after exampleModel ["coffee"] == [])
  , ("Initial delta", after exampleModel [delta] == [0])
  , ("Delta after input", after exampleModel ["coin",delta] == [])
  , ("Delta after output", after exampleModel ["coin","coffee",delta] == [3])
  , ("Tau is internal", after exampleModel [tau] == [])
  , ("Nondeterminism", after branchingModel ["a"] == [1,2])
  , ("Quiescent branch", after branchingModel ["a",delta] == [2])]

-- Test with ordinary traces from LTS.hs
simpleTraceLTS :: LTS
simpleTraceLTS = createLTS [(0, "coin", 1), (1, "coffee", 2)]

testTraces :: Bool
testTraces =
  all (\tr -> not (null (after exampleModel tr))) (traces simpleTraceLTS)

-- Test with suspension traces from Exercise3
testStraces :: Bool
testStraces =
  all (\tr -> not (null (after exampleModel tr))) (straces exampleModel)

-- Test suspension traces on randomly generated IOLTS models
propStracesReachable :: Property
propStracesReachable =
  forAll genValidIOLTS $ \system ->
    all (\tr -> not (null (after system tr))) (straces system)

propInitialReachable :: Property
propInitialReachable =
  forAll genValidIOLTS $ \system@(_, _, _, _, initial) -> initial `elem` after system []

runTests :: IO ()
runTests = do
  mapM_ printTest concreteTests
  printTest ("Ordinary traces", testTraces)
  printTest ("Suspension traces", testStraces)
  putStrLn "\nQuickCheck:"
  quickCheck propStracesReachable
  quickCheck propInitialReachable
  where
    printTest (name, passed) =
      putStrLn (name ++ ": " ++ if passed then "PASS" else "FAIL")


{--
--Test Report/Brief report
-- All 12 concrete tests passed, including traces and suspension traces.
-- Both QuickCheck properties passed 100 tests each.
-- The tests cover tau, delta, nondeterminism and invalid traces.

ghci> runTests
Empty trace: PASS
Tau closure: PASS
Input and output: PASS
Invalid trace: PASS
Initial delta: PASS
Delta after input: PASS
Delta after output: PASS
Tau is internal: PASS
Nondeterminism: PASS
Quiescent branch: PASS
Ordinary traces: PASS
Suspension traces: PASS

QuickCheck:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
ghci> after exampleModel ["coin"]
[1,2]
ghci> straces exampleModel
[[],["coin"],["coin","coffee"],["coin","coffee","delta"],["delta"],["delta","coin"],["delta","coin","coffee"],["delta","delta"],["delta","delta","coin"],["delta","delta","delta"],["delta","delta","delta","coin"],["delta","delta","delta","delta"]]



--}