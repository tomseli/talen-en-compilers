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
  show i = "Ident_ (\"" ++ show' i ++ "\")"
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

compareIdentString :: Ident_ -> String -> Bool
compareIdentString ident s = show ident == "Ident_ (\"" ++ s ++ "\")"

-- Exercise 2

newtype Program = Program Rules deriving (Show)

type Rules = [Rule]

data Rule = Rule Ident_ Cmds deriving (Show)

type Cmds = [Cmd]

data Cmd = CGo 
         | CTake 
         | CMark 
         | CNothing 
         | CTurn   Dir 
         | CCase   Dir    Alts 
         | CIdent Ident_
         deriving (Show)

data Dir = DLeft 
         | DFront 
         | DRight 
         deriving (Show, Enum)

type Alts = [Alt] 

data Alt = Alt Pat Cmds deriving (Show)

data Pat = PEmpty 
         | PLambda 
         | PDebris 
         | PAsteroid 
         | PBoundary 
         | PUnderscore
         deriving (Eq, Show)