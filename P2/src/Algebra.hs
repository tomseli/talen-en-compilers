module Algebra where

import Model
import Data.List (nub, sort, delete)

-- Exercise 5
-- p := program
-- r := rule 
-- c := command
-- a := alt
data ProgAlg p r c a =
  ProgAlg { runProgram    :: [r]           -> p
          , runRule       :: Ident_ -> [c] -> r
          , runGo         :: c
          , runTake       :: c
          , runMark       :: c
          , runNothing    :: c
          , runTurn       :: Dir           -> c
          , runCase       :: Dir    -> [a] -> c
          , runIdent      :: Ident_ ->  c
          , runAlt        :: Pat    -> [c] -> a
          }

doNothing :: a -> b -> ()
doNothing _ _ = ()

fold :: ProgAlg p r c a -> Program -> p
fold (ProgAlg runProgram runRule runGo runTake runMark runNothing runTurn runCase runIdent runAlt)
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
      foldCommand (CIdent ide)     = runIdent ide
      foldAlt     (Alt pat cmds)   = runAlt  pat (map foldCommand cmds)


-- Exercise 6
idAlg :: ProgAlg Program Rule Cmd Alt
idAlg = ProgAlg
  { runProgram = Program
  , runRule    = Rule
  , runGo      = CGo
  , runTake    = CTake
  , runMark    = CMark
  , runNothing = CNothing
  , runTurn    = CTurn
  , runCase    = CCase
  , runIdent   = CIdent
  , runAlt     = Alt
  }

checkStartAlg :: ProgAlg Bool Bool Bool Bool
checkStartAlg = ProgAlg
  { runProgram = or
  , runRule    = \ident _ -> compareIdentString ident "start"
  , runGo      = False
  , runTake    = False
  , runMark    = False
  , runNothing = False
  , runTurn    = const False
  , runCase    = \_ alts -> or alts
  , runIdent   = const False
  , runAlt     = \_ cmds -> or cmds
  }

-- checks for duplicate identifier definition, does not check if it actually gets used etc. 
checkDuplicateAlgs :: ProgAlg Bool [Ident_] [Ident_] ()
checkDuplicateAlgs = ProgAlg
  { runProgram = \xs -> length (nub xs) == length xs -- Care! O(n^2)
  , runRule    = \x xss -> x : concat xss
  , runGo      = []
  , runTake    = []
  , runMark    = []
  , runNothing = []
  , runTurn    = const []
  , runCase    = \_ _ -> []
  , runIdent   = (: [])      -- thanks HLS? equivalent to \x -> x : []
  , runAlt     = doNothing
  }


checkUndefinedAlg :: ProgAlg Bool ([Ident_], [Ident_]) [Ident_] ()
checkUndefinedAlg = ProgAlg
  { runProgram = \xs -> 
    let (defs, calls) = foldr (\(d, c) (x, y) -> (d ++ x, c ++ y)) ([], []) xs
    in nub calls `isSubsetOf` delete (readIdent "start") defs
  , runRule    = \def calls -> ([def], concat calls)
  , runGo      = []
  , runTake    = []
  , runMark    = []
  , runNothing = []
  , runTurn    = const []
  , runCase    = \_ _ -> []
  , runIdent   = (: [])
  , runAlt     = doNothing
  }

checkCasesAlg :: ProgAlg Bool Bool Bool Pat
checkCasesAlg = ProgAlg
  { runProgram = and
  , runRule    = const and
  , runGo      = True
  , runTake    = True
  , runMark    = True
  , runNothing = True
  , runTurn    = const True
  , runCase    = \_ as -> length as == 5 || PUnderscore `elem` as 
  , runIdent   = const True
  , runAlt     = const
  }


-- is A a subset of B?
isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf as bs = foldr (\a res -> a `elem` bs && res) True as

checkProgram :: Program -> Bool
checkProgram p = fold checkStartAlg      p 
              && fold checkDuplicateAlgs p
              && fold checkCasesAlg      p
              && fold checkUndefinedAlg  p