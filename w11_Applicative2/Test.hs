module Test where

import AParser
import SExpr 

main = putStrLn . show . and $ [
    runParser ident "a"             == Just ("a", ""),
    runParser ident "a1"            == Just ( "a1", ""),
    runParser ident "1a"            == Nothing,
    runParser ident "A1a"           == Just ("A1a", ""),
    runParser ident "foo bar"       == Just ("foo", " bar"),
    runParser intOrUppercase "1xxx" == Just ((), "xxx"),
    runParser intOrUppercase "Axxx" == Just ((), "xxx")
    ]