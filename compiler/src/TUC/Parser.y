{
module TUC.Parser (parseString, Unlexer(..), Rule(..)) where

import TUC.Lexer

}

%name parse
%tokentype { Token }
%error { parseError }

%token 
locales          { Locales }
locale_name      { Locale $$ }
','              { Character ',' }
character        { Character $$ }
before           { Before }
after            { After }
space            { Space $$ }
nospace          { NoSpace }

%%

Root : LocaleDecl RuleList { Unlexer $1 $2 }

LocaleDecl    : locales LocaleList { $2 }

LocaleList : locale_name { [$1] }
           | locale_name ',' LocaleList { $1:$3 }

RuleList : Rule { [$1] }
         | Rule RuleList { $1:$2 }

Rule : MaybeSpace before character { BeforeRule $3 $1 }
     | MaybeSpace after character { AfterRule $3 $1 }

MaybeSpace : space { Just $1 }
           | nospace { Nothing }

{
data Unlexer = Unlexer { locales :: [String], rules :: [Rule]}
 deriving (Eq, Show)
data Rule = BeforeRule Char (Maybe Char)
          | AfterRule Char (Maybe Char)
   deriving (Eq, Show)

parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts 

parseString :: String -> Unlexer
parseString = parse . alexScanTokens

}
