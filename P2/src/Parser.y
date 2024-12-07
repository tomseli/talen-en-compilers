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
  ident     { TIdent $$            }

%%

Program : Rules               { Program $1 }

Rules   : Rule                { [$1] }
        | Rule Rules          { $1 : $2 }

Rule    : ident '->' Cmds '.' { Rule $1 $3 }

Cmds    : Cmd                 { [$1] }
        | Cmd ',' Cmds        { $1 : $3 }

Cmd     : go                   { CGo } 
        | take                 { CTake }
        | mark                 { CMark }
        | nothing              { CNothing }
        | turn Dir             { CTurn $2 }
        | case Dir of Alts end { CCase $2 $4 }
        | ident                { CIdent $1 }

Dir     : left                 { DLeft  }
        | right                { DRight }
        | front                { DFront } 

Alts    : Alt                  { [$1] }
        | Alt ';' Alts         { $1 : $3 }           
 
Alt     : Pat '->' Cmds        { Alt $1 $3 }

Pat     : empty                { PEmpty      }
        | lambda               { PLambda     }
        | debris               { PDebris     }
        | asteroid             { PAsteroid   }
        | boundary             { PBoundary   }
        | '_'                  { PUnderscore }

{

happyError _ = error "parse error"

}