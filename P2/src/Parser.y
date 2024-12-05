{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  x { TDot }

%%

Program : { Program }

{

happyError _ = error "parse error"

}