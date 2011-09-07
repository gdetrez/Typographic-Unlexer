{- -*- coding: utf-8 -*- -}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ParserTests where

import Test.Framework
import TUC.Parser
import TUC.Lexer (Token(..))


test_parse_tokens_locales = do
  let source = "Locales fr, fr_fr, fr_be\n"
             ++"NO SPACE before .\n"
             ++"SPACE after é \n"
  assertEqual (parseString source) $
    Unlexer ["fr", "fr_fr", "fr_be"]
            [ BeforeRule '.' Nothing
            , AfterRule 'é' (Just ' ')]

