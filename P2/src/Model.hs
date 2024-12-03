module Model where

-- Exercise 1
data Token = Dot 
           | Comma 
           | Take 
           | Mark 
           | Nothing 
           | Turn 
           | Case 
           | Of 
           | End 
           | Left 
           | Right 
           | Front 
           | Semicolon 
           | Empty 
           | Lambda 
           | Debris 
           | Astroid 
           | Boundary 
           | Underscore 
           | Ident Ident_
           deriving (Eq, Ord, Show)

data Ident_ = Letter Char Ident_ 
            | Digit  Char Ident_
            | Plus        Ident_
            | Minus       Ident_
            | EOI
            deriving (Eq, Ord, Show)

-- Exercise 2
data Program = Program deriving Show
