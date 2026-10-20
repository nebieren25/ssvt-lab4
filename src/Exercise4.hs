
module Exercise4 where

import LTS

-- Find states reachable with one visible action
nextStates :: IOLTS -> [State] -> Label -> [State]
nextStates (_, _, _, transitions, _) states action =
  makeSet
    [ target
    | (source, label, target) <- transitions
    , source `elem` states
    , label == action
    ]

-- Find all states reachable through zero or more tau steps
tauClosure :: IOLTS -> [State] -> [State]
tauClosure (_, _, _, transitions, _) initial =
  go (makeSet initial)
  where
    go states =
      let reachable =
            makeSet
              (states ++
                [ target
                | (source, label, target) <- transitions
                , source `elem` states
                , label == tau
                ])
      in if reachable == states
         then states
         else go reachable

-- Check whether a state is quiescent
isQuiescent :: IOLTS -> State -> Bool
isQuiescent (_, _, outputs, transitions, _) state =
  not (any isActive transitions)
  where
    isActive (source, label, _) =
      source == state &&
      (label == tau || label `elem` outputs)

-- TODO: Combine the helpers into the after function
after :: IOLTS -> Trace -> [State]
after system trace =
  error "TODO: Implement after"
