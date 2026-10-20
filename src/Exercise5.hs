
module Exercise5 where

import LTS
import Exercise4 (after, isQuiescent)
import Control.Exception (SomeException, try, evaluate)

--own code

doorCommands :: [Label]
doorCommands = ["close", "open", "lock", "unlock"]

-- Model of the correct door behavior
doorIOLTS :: IOLTS
doorIOLTS = createIOLTS
  [ (0, "?close", 3), (3, "!closed", 1)
  , (1, "?open", 4), (4, "!opened", 0)
  , (1, "?lock", 5), (5, "!locked", 2)
  , (2, "?unlock", 6), (6, "!unlocked", 1)
  ]

doorModel :: LTS
doorModel = (states, inputs ++ outputs, transitions, initial)
  where
    (states, inputs, outputs, transitions, initial) = doorIOLTS

-- Separate input and output labels for use with after
toIOLTS :: LTS -> IOLTS
toIOLTS (states, labels, transitions, initial) =
  (states, inputs, outputs, transitions, initial)
  where
    inputs = filter (`elem` doorCommands) labels
    outputs = filter (`notElem` doorCommands) labels

-- Possible outputs after a trace, including quiescence
outputsAfter :: IOLTS -> Trace -> [Label]
outputsAfter system@(_, _, outputs, transitions, _) trace =
  makeSet (visibleOutputs ++ quiescence)
  where
    reachable = after system trace

    visibleOutputs =
      [label | (source, label, _) <- transitions,
               source `elem` reachable, label `elem` outputs]

    quiescence = [delta | any (isQuiescent system) reachable]

-- Explore possible command sequences and report the first mismatch
findFailure :: LTS -> (State -> Label -> (State, Label)) -> Maybe String
findFailure model sut = explore [] [([], 0)]
  where
    specification = toIOLTS model

    explore _ [] = Nothing
    explore visited ((trace, sutState):pending)
      | pair `elem` visited = explore visited pending
      | otherwise =
          case checkCommands trace sutState commands of
            Left message -> Just message
            Right next -> explore (pair:visited) (pending ++ next)
      where
        pair = (after specification trace, sutState)
        commands =
          [command | command <- doorCommands,
                     not (null (after specification (trace ++ [command])))]

    checkCommands _ _ [] = Right []
    checkCommands trace sutState (command:remaining) =
      if actualOutput `elem` expected && not (null (after specification fullTrace))
      then
        case checkCommands trace sutState remaining of
          Left message -> Left message
          Right next -> Right ((fullTrace, newState):next)
      else
        Left ("Trace: " ++ show trace ++
              ", command: " ++ show command ++
              ", expected: " ++ show expected ++
              ", received: " ++ show actualOutput)
      where
        inputTrace = trace ++ [command]
        expected = outputsAfter specification inputTrace
        (newState, actualOutput) = sut sutState command
        fullTrace = inputTrace ++ [actualOutput]

-- Check whether the implementation conforms to the model
testLTSAgainstSUT :: LTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT model sut =
  case findFailure model sut of
    Nothing -> True
    Just _ -> False

-- Test all provided door implementations
runTests :: IO ()
runTests = mapM_ check implementations
  where
    implementations =
      [ ("doorImpl1", doorImpl1, True)
      , ("doorImpl2", doorImpl2, False)
      , ("doorImpl3", doorImpl3, False)
      , ("doorImpl4", doorImpl4, False)
      , ("doorImpl5", doorImpl5, False)
      , ("doorImpl6", doorImpl6, False)
      , ("doorImpl7", doorImpl7, False)
      , ("doorImpl8", doorImpl8, False)
      ]

    check (name, sut, expected) = do
      result <- try (evaluate (findFailure doorModel sut))
        :: IO (Either SomeException (Maybe String))

      let conforms =
            case result of
              Right Nothing -> True
              _ -> False

          status = if conforms == expected then "PASS" else "FAIL"

      putStrLn (status ++ " - " ++ name ++
                if conforms then " (conforms)" else " (rejected)")

      case result of
        Right (Just message) -> putStrLn ("  " ++ message)
        Left err -> putStrLn ("  SUT error: "++ takeWhile (/= '\n') (show err))
        Right Nothing -> return ()


{--
--Test Report/Brief report
-- All 8 implementation tests passed.
-- doorImpl1 conformed to the specification.
-- doorImpl2-8 were correctly rejected due to wrong outputs or errors.
""""""
ghci> testLTSAgainstSUT doorModel doorImpl1
True
ghci> testLTSAgainstSUT doorModel doorImpl2
False

ghci> runTests
PASS - doorImpl1 (conforms)
PASS - doorImpl2 (rejected)
  Trace: [], command: "close", expected: ["closed"], received: "opened"
PASS - doorImpl3 (rejected)
  SUT error: Invalid command and/or state!
PASS - doorImpl4 (rejected)
  SUT error: Invalid command and/or state!
PASS - doorImpl5 (rejected)
  Trace: ["close","closed"], command: "open", expected: ["opened"], received: "open"
PASS - doorImpl6 (rejected)
  SUT error: Door is stuck!
PASS - doorImpl7 (rejected)
  SUT error: Incorrect keycode!
PASS - doorImpl8 (rejected)
  SUT error: Invalid command and/or state!



--}