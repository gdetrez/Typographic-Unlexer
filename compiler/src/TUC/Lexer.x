{
module TUC.Lexer (alexScanTokens, Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		   -- alphabetic characters

@locale = [a-z]{2} ( "_" [a-z]{2} )?

tokens :-

-- Skipping white spaces
$white+				        ;
-- Skipping comments
"#".*					;

-- Locale names
@locale                                 { \s -> Locale s }

-- Characters
-- parsing a character given its unicode code point
"U+" [A-F0-9]{1,4}                      { \s -> Character $ readCodePoint s }
$printable                              { \s -> Character $ readCharacter s }

-- Keywords
"Locales"                               { \s -> Locales }
"before"                                { \s -> Before }
"after"                                 { \s -> After }

"NO SPACE"                              { \s -> NoSpace }
"SPACE"                                 { \s -> Space ' ' }
"NO-BREAK" $white+ "SPACE"              { \s -> Space '\xa0' }

"EN" $white+ "QUAD"                     { \s -> Space '\x2000' }
"EM" $white+ "QUAD"                     { \s -> Space '\x2001' }
"EN" $white+ "SPACE"                    { \s -> Space '\x2002' }
"EM" $white+ "SPACE"                    { \s -> Space '\x2003' }
"THREE-PER-EM" $white+ "SPACE"          { \s -> Space '\x2004' }
"FOUR-PER-EM" $white+ "SPACE"           { \s -> Space '\x2005' }
"SIX-PER-EM" $white+ "SPACE"            { \s -> Space '\x2006' }
"FIGURE" $white+ "SPACE"                { \s -> Space '\x2007' }
"PUNCTUATION" $white+ "SPACE"           { \s -> Space '\x2008' }
"THIN" $white+ "SPACE"                  { \s -> Space '\x2009' }
"HAIR" $white+ "SPACE"                  { \s -> Space '\x200A' }
"ZERO" $white+ "WIDTH" $white+ "SPACE"  { \s -> Space '\x200B' }

"NARROW" $white+ "NO-BREAK" $white+ "SPACE"
                                        { \s -> Space '\x202F' }
"MEDIUM" $white+ "MATHEMATICAL" $white+ "SPACE"
                                        { \s -> Space '\x205F' }
"IDEOGRAPHIC" $white+ "SPACE"           { \s -> Space '\x3000' }
"ZERO" $white+ "WIDTH" $white+ "NO-BREAK" $white+ "SPACE"
                                        { \s -> Space '\xFEFF' }


{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    Locales
  | Locale String
  -- Rules
  | NoSpace
  | Space Char
  -- Directions
  | Before
  | After  
  -- Character
  | Character Char
  deriving (Eq,Show)


lexer = do
  s <- getContents
  print (alexScanTokens s)

readCodePoint :: String -> Char
readCodePoint ('U':'+':code) = read $ "'\\x" ++ code ++ "'"
readCodePoint s =
  error $ "Given string '" ++ s ++ "' is not a valid unicode codepoint"

readCharacter :: String -> Char
readCharacter [c] = c
readCharacter s =
  error $ "Single character expected, got '" ++ s ++ "'"
}
