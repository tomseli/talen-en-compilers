module Algebra where

import Model


-- Exercise 5
-- p := program
-- r := rule 
-- c := command
-- d := direction
-- a := alt
-- x := pattern
data ProgAlg p r c d a = 
  ProgAlg { runProgram    :: [r]           -> p
          , runRule       :: Ident_ -> [c] -> r
          , runGo         :: c
          , runTake       :: c
          , runMark       :: c 
          , runNothing    :: c
          , runTurn       :: Dir           -> c
          , runCase       :: Dir    -> [a] -> c
          , runAlt        :: Pat    -> [c] -> a 
          }

fold :: ProgAlg p r c d a -> Program -> p
fold (ProgAlg runProgram runRule runGo runTake runMark runNothing runTurn runCase runAlt) 
  = foldProgram
    where 
      foldProgram (Program rules)  = runProgram (map foldRule rules)
      foldRule    (Rule ide cmds)  = runRule ide (map foldCommand cmds)
      foldCommand CGo              = runGo
      foldCommand CTake            = runTake
      foldCommand CMark            = runMark
      foldCommand CNothing         = runNothing
      foldCommand (CTurn dir)      = runTurn dir
      foldCommand (CCase dir alts) = runCase dir (map foldAlt alts)
      foldAlt     (Alt pat cmds)   = runAlt  pat (map foldCommand cmds)


-- Exercise 6
-- idAlg :: ProgAlg (Program [Rules]) 
-- idAlg = ProgAlg { runProgram = id}


checkProgram :: Program -> Bool
checkProgram = undefined