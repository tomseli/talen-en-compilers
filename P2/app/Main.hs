module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

import ParseLib
import Data.Maybe
import System.IO


import qualified Data.Map as L

run :: Parser a b -> [a] -> Maybe b
run p s = case parse p s of
  (a, []):_  -> Just a
  _          -> Nothing

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env arrow = do
  getChar
  let x = step env arrow
  printArrowIO arrow
  case x of
    (Ok a)       -> interactive env a
    (Done a b c) -> putStrLn "Done"
    (Fail s)     -> error s

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env arrow = do
  let x = step env arrow
  case x of
    (Ok a)       -> batch env a
    (Done a b c) -> (a, b, c)
    (Fail s)     -> error s

main :: IO ()
main = do
  putStrLn "Specify a .space file:"
  spaceFP <- getLine

  putStrLn "Specify a .Arrow file:"
  arrowFP <- getLine

  chars <- readFile arrowFP
  let env = toEnvironment chars
  spacechars <- readFile spaceFP
  let space = fromJust $ run parseSpace spacechars
  printSpaceIO space

  let ((my, mx), _) = L.findMax space

  putStrLn "Specify an x coordinate:"
  x <- getLine
  let nx = read x :: Int

  putStrLn "Specify a  y coordinate:"
  y <- getLine
  let ny = read y :: Int

  if nx < 0 || nx > mx || ny < 0 || ny > my 
  then error "Input coordinates are invalid" 
  else putStrLn "Press 1 for batch mode, press anything else for interactive mode"
  chosen <- getLine  
  let nr = read chosen :: Int

  if nr == 1 
  then do
    let (a, b, c) = batch env (ArrowState space (my - ny, nx) Up_ [CIdent $ readIdent "start"])
    putStrLn "\n Done!"
    printSpaceIO a
  else do
    putStrLn "= Press enter to continue ="
    interactive env (ArrowState space (my - ny, nx) Up_ [CIdent $ readIdent "start"])


printEnvIO :: Environment -> IO ()
printEnvIO env = do
  putStrLn "Environment:" 
  mapM_ printKeyValue (L.toList env)
  putStrLn ""
    where
      printKeyValue (key, value) = putStrLn $ show key ++ " -> " ++ show value

printSpaceIO :: Space -> IO ()
printSpaceIO s = do 
  putStrLn "Space:"
  putStrLn (printSpace s)
  putStrLn ""

printArrowIO :: ArrowState -> IO ()
printArrowIO (ArrowState s (x,y) h cs) = do
  putStrLn "= Arrow ="
  printSpaceIO s
  putStrLn ("Position: " ++ "(" ++ show y ++ "," ++ show (mx - x) ++ ")")
  putStrLn ("Heading: " ++ show h)
  putStrLn ("Stack: " ++ show cs)
  putStrLn "= Press enter to continue ="
  where 
    ((_, mx), _) = L.findMax s

