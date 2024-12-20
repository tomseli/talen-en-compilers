module Interpreter where

import ParseLib

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace)
import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)

import Lexer
import Parser
import Model
import Algebra

data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving (Eq, Ord)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces

-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )
                 ]


-- Exercise 7
contentsMap :: L.Map Contents Char
contentsMap = L.fromList contentsTable  -- Correctly mapping the contents to their characters

printSpace :: Space -> String
printSpace space = unlines $ map printRow [0..maxX]  
  where
    -- Find the maximum x and y values in the space
    maxX = maximum (map fst (L.keys space))
    maxY = maximum (map snd (L.keys space))

    -- Print each row as a string
    printRow x = [fromMaybe '?' (L.lookup (x, y) space >>= (`L.lookup` contentsMap)) | y <- [0..maxY]]

-- These three should be defined by you
data Direction = Left_ | Up_ | Right_ | Down_ deriving (Show, Eq, Enum)

turnRight :: Direction -> Direction
turnRight dir = toEnum ((fromEnum dir + 1) `mod` 4) :: Direction

turnLeft :: Direction -> Direction 
turnLeft dir = toEnum ((fromEnum dir - 1) `mod` 4) :: Direction

addDirection :: Direction -> Dir -> Direction 
addDirection direction dir = toEnum ((fromEnum direction + (fromEnum dir - 1)) `mod` 4) :: Direction

type Ident = Ident_
type Commands = Cmds
type Heading = Direction

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String 

initArrowState :: Space -> ArrowState
initArrowState s = ArrowState s (0,0) Up_ [CIdent $ readIdent "start"]

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s = if checkProgram p
                  then foldr f L.empty rs
                  else error "toEnvironment: Program failed check"
  where
    p@(Program rs)      = (parser . alexScanTokens) s
    f (Rule ident cs) = L.insert ident cs

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step env (ArrowState s pos h [])       = Done s pos h
step env (ArrowState s pos h (cmd:cs)) = case cmd of 
  CGo             -> Ok $ stepGo   newState
  CTake           -> Ok $ stepTake newState
  CMark           -> Ok $ stepMark newState
  CNothing        -> Ok $ ArrowState s pos h cs -- consume from stack and move on
  CTurn  dir      -> Ok $ stepTurn dir newState
  CCase  dir alts -> Ok $ stepCase dir alts newState
  CIdent ident    -> Ok $ stepIdent ident env newState
  where 
    newState = ArrowState s pos h cs

stepIdent :: Ident_ -> Environment -> ArrowState -> ArrowState
stepIdent ident env (ArrowState s pos h cs) = ArrowState s pos h (env L.! ident ++ cs) 

stepCase :: Dir -> Alts -> ArrowState -> ArrowState 
stepCase dir alts a@(ArrowState s pos h cs) 
 | convertPat scanned `elem` patterns  = ArrowState s pos h $ findCmds (convertPat scanned) alts ++ cs
 -- give priority to everything but underscores
 | PUnderscore `elem` patterns         = ArrowState s pos h $ findCmds PUnderscore alts          ++ cs
 | otherwise                           = error "stepCase: Failed to find a matching pattern"
  where 
    scanned = scanDir dir a
    patterns = map (\(Alt pat _) -> pat) alts

convertPat :: Contents -> Pat
convertPat Empty    = PEmpty 
convertPat Lambda   = PLambda 
convertPat Debris   = PDebris 
convertPat Asteroid = PAsteroid 
convertPat Boundary = PBoundary 

scanDir :: Dir -> ArrowState -> Contents
scanDir dir (ArrowState s (x, y) h cs) | dirToScan == Left_  = fromMaybe Boundary (s L.!? (x-1, y  ))
                                       | dirToScan == Up_    = fromMaybe Boundary (s L.!? (x  , y+1))
                                       | dirToScan == Right_ = fromMaybe Boundary (s L.!? (x+1, y  ))
                                       | dirToScan == Down_  = fromMaybe Boundary (s L.!? (x  , y-1))
  where 
    dirToScan = addDirection h dir

findCmds :: Pat -> [Alt] -> [Cmd]
findCmds t as = concat [cmds | (Alt pat cmds) <- as, pat == t]

stepTurn :: Dir -> ArrowState -> ArrowState
stepTurn dir (ArrowState s pos h cs) = case dir of 
  DLeft  -> newState (turnLeft h) 
  DRight -> newState (turnRight h) 
  _      -> ArrowState s pos h cs -- do nothing!
  where 
    newState x = ArrowState s pos x cs  

stepMark :: ArrowState -> ArrowState
stepMark (ArrowState s pos h cs) = ArrowState (L.insert pos Lambda s) pos h cs 

stepTake :: ArrowState -> ArrowState
stepTake (ArrowState s pos h cs) = case s L.!? pos of 
  Just Lambda -> ArrowState (L.insert pos Empty s) pos h cs  
  Just Debris -> ArrowState (L.insert pos Empty s) pos h cs 
  Just _      -> ArrowState s pos h cs 
  Nothing     -> ArrowState s pos h cs 

stepGo :: ArrowState -> ArrowState
stepGo (ArrowState s pos Left_  cs) = ArrowState s (stepPos s ( 0,-1) pos) Left_  cs
stepGo (ArrowState s pos Right_ cs) = ArrowState s (stepPos s ( 0, 1) pos) Right_ cs
stepGo (ArrowState s pos Up_    cs) = ArrowState s (stepPos s (-1, 0) pos) Up_    cs
stepGo (ArrowState s pos Down_  cs) = ArrowState s (stepPos s ( 1, 0) pos) Down_  cs
    
stepPos :: Space -> (Int, Int) -> Pos -> Pos
stepPos space (dy, dx) (y, x) = case space L.!? move of
  Just Lambda -> move -- accepted move
  Just Debris -> move -- accepted move
  Just Empty  -> move -- accepted move
  Nothing     -> stay -- Boundary
  Just _      -> stay -- Anything else is not accepted
  where 
    ((my, mx), _) = L.findMax space
    move = (y + dy, x + dx)
    stay = (y, x)  