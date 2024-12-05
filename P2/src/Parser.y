{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  '->'      { TArrow               }
  '.'       { TDot                 }
  ','       { TComma               }
  go        { TGo                  }
  take      { TTake                }
  mark      { TMark                }
  turn      { TTurn                }
  nothing   { TNothing             }
  case      { TCase                }
  of        { TOf                  }
  end       { TEnd                 }
  left      { TLeft                }
  right     { TRight               }
  front     { TFront               }
  ';'       { TSemicolon           }
  empty     { TEmpty               }
  lambda    { TLambda              }
  debris    { TDebris              }
  asteroid  { TAsteroid            }
  boundary  { TBoundary            }
  '_'       { TUnderscore          }

%%

Program : { Program [] }

Rule    : ident '->' Cmds '.' { Rule $1 $2 }

Cmds    : Cmd                 { Cmds [$1] }
        | Cmds Cmd            { $1 : $2 }

Cmd     : go 
        | take 
        | mark
        | nothing 
        | turn Dir 
        | case Dir Alts 
        | -- identifier?

Dir     : left 
        | right
        | front

{

happyError _ = error "parse error"

}