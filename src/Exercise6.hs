
module Exercise6 where

import LTS
import Exercise2 (genValidIOLTS)
import Test.QuickCheck (Gen, generate)

--own code

-- Display states and transitions as an ASCII diagram
visualizeLTS :: IOLTS -> IO ()
visualizeLTS (states, inputs, outputs, transitions, initial) = do
  putStrLn "\n========== LTS Visualization =========="
  putStrLn ("Initial state: " ++ show initial)
  putStrLn ""
  mapM_ showState states
  putStrLn "=======================================\n"
  where
    showState state = do
      let marker = if state == initial then "-> " else "   "
      putStrLn (marker ++ "[" ++ show state ++ "]")

      let outgoing = [(label, to) | (from, label, to) <- transitions, from == state]

      if null outgoing
        then putStrLn "      (no outgoing transitions)"
        else mapM_ showTransition outgoing

      putStrLn ""

    showTransition (label, to) =
      putStrLn ("      +-- " ++ formatLabel label ++ " --> [" ++ show to ++ "]")

    -- Add input/output prefixes only when missing
    formatLabel label
      | label == tau = "tau"
      | label `elem` inputs = if take 1 label == "?" then label else "?" ++ label
      | label `elem` outputs = if take 1 label == "!" then label else "!" ++ label
      | otherwise = label

-- Generate a random IOLTS and visualize it
visualizeRandomLTS :: Gen IOLTS -> IO ()
visualizeRandomLTS generator = do
  randomLTS <- generate generator
  visualizeLTS randomLTS

-- Example model for testing
exampleLTS :: IOLTS
exampleLTS = createIOLTS [(0, "?coin", 1), (1, tau, 2), (2, "!coffee", 3), (3, "?reset", 0)]

runExample :: IO ()
runExample = visualizeLTS exampleLTS

runRandomExample :: IO ()
runRandomExample = visualizeRandomLTS genValidIOLTS


{--
-- Tested with a predefined IOLTS and a model containing a terminal state.
-- Also tested with a randomly generated IOLTS from Exercise2.
All three visualizations displayed states and transitions as expected.

"ghci> runExample

========== LTS Visualization ==========
Initial state: 0

-> [0]
      +-- ?coin --> [1]

   [1]
      +-- tau --> [2]

   [2]
      +-- !coffee --> [3]

   [3]
      +-- ?reset --> [0]

=======================================

ghci> visualizeLTS coffeeImpl1

========== LTS Visualization ==========
Initial state: 1

-> [1]
      +-- ?coin --> [2]

   [2]
      +-- !coffee --> [3]

   [3]
      (no outgoing transitions)

=======================================

ghci> runRandomExample

========== LTS Visualization ==========
Initial state: 1

-> [1]
      +-- !w --> [5]
      +-- ?b --> [1]

   [2]
      +-- ?b --> [1]

   [3]
      (no outgoing transitions)

   [4]
      (no outgoing transitions)

   [5]
      (no outgoing transitions)

   [6]
      (no outgoing transitions)

   [7]
      (no outgoing transitions)

   [8]
      +-- !w --> [4]

=======================================


"


--}