module Lib where

import Text.Parsec hiding (State)
import Text.Parsec.String
import qualified Data.Map as M
import Data.Char
import Control.Monad.Identity
import Control.Monad.State

chineseToHaskell :: String -> String
chineseToHaskell xs = case parse whole "" xs of 
    Left err -> error (show err)
    Right res -> translate res


translate :: [String] -> String -- chinese to english translation
translate [] = ""
translate (x:xs) = case globalCToETable M.!? x of 
    Nothing -> if length x == 1 && isAlphaNum (head x) then x else error $ "No matching for token " ++ show x
    Just match -> match ++ " " ++ translate xs


whole :: Parser [String]
whole = many1 ( try underLinedKeyWord <|> singleCharaterKeyword) <* eof

singleCharaterKeyword :: Parser String
singleCharaterKeyword  = do
    x <- anyChar
    return [x]

underLinedKeyWord :: Parser String -- parses 
underLinedKeyWord = do
    string "——"
    xs <- manyTill anyChar (try( string "——"))
    return xs

globalCToETable :: M.Map String String
globalCToETable = fst $ runIdentity $ execStateT constructTable (M.empty, M.empty) 

type TranslateTable = State (M.Map String String, M.Map String String) ()

(|-:) :: String -> String ->  TranslateTable -- forward map, reverse map
s1 |-: s2 = state $ \(cTe, eTc) ->
    let ncTe = M.insert s1 s2 cTe
        neTc = M.insert s2 s1 eTc
    in ((ncTe, neTc), ())

 
constructTable :: TranslateTable
constructTable = do
    "入"         |-:    "import"
    "控制"       |-:    "Control"
    "。"         |-:    "."
    "其类型为"   |-:    "::"
    "（"         |-:    "("
    "）"         |-:    ")"
    "等于"       |-:    "="
    "得"         |-:    "->"
    "【"         |-:    "["
    "】"         |-:    "]"
    "："         |-:    ":"
    "字符串"     |-:    "String"
    "整数"       |-:    "Integer"
    "合并"       |-:    "++"
    "甲"         |-:    "a"
    "乙"         |-:    "b"
    "丙"         |-:    "c"
    "丁"         |-:    "d"
    "加"         |-:    "+"
    "减"         |-:    "-"
    "乘"         |-:    "*"
    "除"         |-:    "/"

