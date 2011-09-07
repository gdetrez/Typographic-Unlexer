{- -*- coding: utf-8 -*- -}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module LexerTests where

import Test.Framework
import TUC.Lexer (alexScanTokens, Token(..))

-- Testing single tokens
test_locale = do
  assertEqual [Locale "fr_fr"] (alexScanTokens "fr_fr")
  assertEqual [Locale "fr"] (alexScanTokens "fr")


test_keywords = do
  assertEqual [Locales] (alexScanTokens "Locales")
  assertEqual [Before] (alexScanTokens "before")
  assertEqual [After] (alexScanTokens "after")
  assertEqual [Character ','] (alexScanTokens ",")

test_space_names = 
  mapM testOne 
  [ ('\x0020', "SPACE")
  , ('\x00A0', "NO-BREAK SPACE")
  , ('\x2000', "EN QUAD")
  , ('\x2001', "EM QUAD")
  , ('\x2002', "EN SPACE")
  , ('\x2003', "EM SPACE")
  , ('\x2004', "THREE-PER-EM SPACE")
  , ('\x2005', "FOUR-PER-EM SPACE")
  , ('\x2006', "SIX-PER-EM SPACE")
  , ('\x2007', "FIGURE SPACE")
  , ('\x2008', "PUNCTUATION SPACE")
  , ('\x2009', "THIN SPACE")
  , ('\x200A', "HAIR SPACE")
  , ('\x200B', "ZERO WIDTH SPACE")
  , ('\x202F', "NARROW NO-BREAK SPACE")
  , ('\x205F', "MEDIUM MATHEMATICAL SPACE")
  , ('\x3000', "IDEOGRAPHIC SPACE")
  , ('\xFEFF', "ZERO WIDTH NO-BREAK SPACE")
  ]
    where testOne (char, name) =
            assertEqual [Space char] (alexScanTokens name)

test_nospace = assertEqual [NoSpace] (alexScanTokens "NO SPACE")

test_character_litteral = do
  -- basic caracter: full stop
  assertEqual [Character '.'] (alexScanTokens ".")
  -- Unicode caracter: coptic full stop
  assertEqual [Character '⳾'] (alexScanTokens "⳾")

test_character_codepoints = do
  -- basic caracter: full stop
  assertEqual [Character '.'] (alexScanTokens "U+002E")
  assertEqual [Character '.'] (alexScanTokens "U+2E")
  -- Unicode caracter: coptic full stop
  assertEqual [Character '⳾'] (alexScanTokens "U+2CFE")

-- Testing lines
test_local_decl = 
  assertEqual [Locales, Locale "fr", Character ',', Locale "fr_fr"]
              (alexScanTokens "Locales fr, fr_fr")

test_rules = do
  assertEqual
    (alexScanTokens "NO SPACE before ⳾")
    [NoSpace, Before, Character '⳾']
  assertEqual
    (alexScanTokens "NO-BREAK SPACE after ,")
    [Space '\x00A0', After, Character ',']
  assertEqual
    (alexScanTokens "SPACE before -")
    [Space ' ', Before, Character '-']
  assertEqual
    (alexScanTokens "HAIR SPACE before H")
    [Space '\x200A', Before, Character 'H']
