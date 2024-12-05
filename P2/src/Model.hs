{-# LANGUAGE InstanceSigs #-}
module Model where

import Data.Char

-- Exercise 1
data Token = TArrow
           | TDot 
           | TComma 
           | TGo
           | TTake 
           | TMark 
           | TNothing 
           | TTurn 
           | TCase 
           | TOf 
           | TEnd 
           | TLeft 
           | TRight 
           | TFront 
           | TSemicolon 
           | TEmpty 
           | TLambda 
           | TDebris 
           | TAsteroid 
           | TBoundary 
           | TUnderscore 
           | TIdent Ident_
           deriving (Eq, Ord, Show)

data Ident_ = Letter Char Ident_ 
            | Digit  Char Ident_
            | Plus        Ident_
            | Minus       Ident_
            | EOI
            deriving (Eq, Ord)

instance Show Ident_ where 
  show :: Ident_ -> String
  show i = "Ident_ ( \"" ++ show' i ++ "\" )"
    where 
      show' x = case x of 
        (Letter c rest) -> c : show' rest   
        (Digit  c rest) -> c : show' rest   
        (Plus     rest) -> "+" ++ show' rest   
        (Minus    rest) -> "-" ++ show' rest
        EOI -> ""    

-- writing an instance for Read is apparently very difficult
-- so I will leave it at this
readIdent :: String -> Ident_ 
readIdent []     = EOI
readIdent (s:ss) | isAlpha s  = Letter s (readIdent ss) 
                 | isNumber s = Digit s  (readIdent ss)
                 | '+' == s   = Plus     (readIdent ss)
                 | '-' == s   = Minus    (readIdent ss)
                 | otherwise  = error "readIdent: Failed to match"

-- Exercise 2

newtype Program = Program [Rule] deriving (Show)

data Rule = Rule Ident_ Cmds deriving (Show)

newtype Cmds = Cmds [Cmd] deriving (Show)

data Cmd = CGo 
         | CTake 
         | CMark 
         | CNothing 
         | CTurn   Dir 
         | CCase   Dir    Alts 
         | CIndent Ident_
         deriving (Show)

data Dir = Left 
         | Right 
         | Front  
         deriving (Show)

newtype Alts = Alts [Alt] deriving (Show)

data Alt = Alt Pat Cmds deriving (Show)

data Pat = PEmpty 
         | PLambda 
         | PDebris 
         | PAsteroid 
         | PBoundary 
         | PUnderscore
         deriving (Show) 
