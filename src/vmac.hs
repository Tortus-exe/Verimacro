{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (lexeme)
import Data.Void
import Control.Monad.State.Strict
import Data.Text (Text, pack, unpack, singleton)
import qualified Data.Text as T

data Braces = ModOpen | CaseOpen | BeginOpen deriving(Show)

type Parser = ParsecT Void Text (State [Braces])

ignoreWS :: Parser a -> Parser a
ignoreWS = lexeme space1

modl :: Parser Text
modl = do
    _ <- ignoreWS $ string "mod"
    name <- ignoreWS $ some alphaNumChar
    _ <- char '('
    braceList <- lift get
    put $ [ModOpen] ++ braceList
    return $ T.concat ["module ", pack name, " ("]

input :: Parser Text
input = string "i " *> return "input "

logic :: Parser Text
logic = string "l " *> return "logic "

regBounds :: Parser Text
regBounds = do
    a <- between "[" "]" $ some digitChar
    return $ T.concat ["[", pack a, ":0]"]

assign :: Parser Text
assign = string "-> " *> return "assign "

ff :: Parser Text
ff = do
    _ <- ignoreWS $ string "ff"
    _ <- string "("
    braceList <- lift get
    put $ [BeginOpen] ++ braceList
    return "always_ff("

cond :: Parser Text
cond = do
    _ <- ignoreWS $ string "if"
    _ <- string "("
    braceList <- lift get
    put $ [BeginOpen] ++ braceList
    return "if ("

comb :: Parser Text
comb = do
    _ <- ignoreWS $ string "comb"
    _ <- string "{"
    braceList <- lift get
    put $ [BeginOpen] ++ braceList
    return "always_comb begin "

caseo :: Parser Text
caseo = do
    _ <- ignoreWS $ string "case"
    _ <- string "("
    braceList <- lift get
    put $ [CaseOpen] ++ braceList
    return "case ("

openCurly :: Parser Text
openCurly = do
    _ <- char '{'
    (c:_) <- lift get
    let replacement = case c of
                        ModOpen -> ";"
                        CaseOpen -> ""
                        BeginOpen -> "begin"
    return replacement

closeCurly :: Parser Text
closeCurly = do
    _ <-  char '}'
    k <- lift get
    if null k then empty else do
        (c:cs) <- lift get
        put cs
        case c of
            ModOpen -> return "endmodule"
            CaseOpen -> return "endcase"
            BeginOpen -> return "end"

defcase :: Parser Text
defcase = ignoreWS $ string "def>>" *> return "default: "

tlParser :: Parser Text
tlParser = T.concat <$> some (
                try modl <|>
                try input <|> 
                try logic <|>
                try assign <|> 
                try openCurly <|>
                try closeCurly <|>
                try comb <|>
                try cond <|>
                try ff <|>
                try caseo <|>
                try defcase <|> 
                try regBounds <|>
                (singleton <$> anySingle)
                             )

handleFileNameParseError :: Either (ParseErrorBundle String Void) String -> String
handleFileNameParseError (Left k) = error $ errorBundlePretty $ k
handleFileNameParseError (Right x) = x

parseAndWrite :: [String] -> IO ()
parseAndWrite [] = error "no file given!"
parseAndWrite (k:[]) = do
    b <- readFile k
    let (c, _) = runState (runParserT tlParser "" $ pack b) []
        outputFileName = (++".v") . handleFileNameParseError . parse (takeWhileP Nothing (/= '.')::Parsec Void String String) "" $ k
    case c of
        Left e -> putStrLn . errorBundlePretty $ e
        Right x -> writeFile outputFileName $ unpack x
parseAndWrite (k:"-o":v:[]) = do
    b <- readFile k
    let (c, _) = runState (runParserT tlParser "" $ pack b) []
    case c of
        Left e -> putStrLn . errorBundlePretty $ e
        Right x -> writeFile v $ unpack x
parseAndWrite _ = putStrLn "invalid usage!\nusage: vmac input/file/path.vm [-o output-file/path.v]"

main :: IO ()
main = parseAndWrite =<< getArgs