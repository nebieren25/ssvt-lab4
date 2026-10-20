
module Exercise6 where

import LTS
import Test.QuickCheck (Gen, generate)

--own code

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

      let outgoing =
            [ (label, to)
            | (from, label, to) <- transitions
            , from == state
            ]

      if null outgoing
        then putStrLn "      (no outgoing transitions)"
        else mapM_ showTransition outgoing

      putStrLn ""

    showTransition (label, to) =
      putStrLn
        ("      +-- " ++ formatLabel label
        ++ " --> [" ++ show to ++ "]")

    -- Restore input/output prefixes removed by createIOLTS.
    formatLabel label
      | label == tau = "tau"
      | label `elem` inputs = "?" ++ label
      | label `elem` outputs = "!" ++ label
      | otherwise = label


-- Generate a random IOLTS and visualize it.
visualizeRandomLTS :: Gen IOLTS -> IO ()
visualizeRandomLTS generator = do
  randomLTS <- generate generator
  visualizeLTS randomLTS


-- Example for testing without the random generator.
exampleLTS :: IOLTS
exampleLTS =
  createIOLTS
    [ (0, "?coin", 1) , (1, tau, 2) , (2, "!coffee", 3) , (3, "?reset", 0)]

runExample :: IO ()
runExample = visualizeLTS exampleLTS


--Test Report/Brief report
-- The example covers input, output and tau transitions.
-- The visualizer also handles states without outgoing transitions.
-- Random generation can be tested with Exercise 2's ltsGen.
-- Execution results have not yet been recorded.

--Design choices
-- ASCII visualization avoids external graphics dependencies.
-- States and outgoing transitions are grouped together.
-- The initial state is marked with an arrow.
-- Input/output labels retain their ? and ! prefixes.
-- QuickCheck's generate is reused for random IOLTS models.

-- Time Spent: X min
