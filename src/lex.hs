import System.IO
import System.Environment
import Control.Monad
import Data.Function
import Data.List

data BraceMod = Default | Module | ModuleOpen | Case | CaseOpen

whitespace = " \n\t\r"
wsAsList = [" ","\n","\t","\r"]
specialChars = "()[]{}"
semicolon = ";"
alphanumeric = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

wordsSpecial :: String -> [String]
wordsSpecial x = foldr (++) [] $
                 map (groupBy (allin semicolon)) $
                 foldr (++) [] $ 
                 map (groupBy (allin whitespace)) $
                 groupBy (allin specialChars) x
        where
             allin k = ((xor) `on` (`elem` k))
             xor a b = (a && b) || ((not a) && (not b))

innerUnmacro :: String -> String
innerUnmacro [] = []
innerUnmacro (']':']':[]) = ":0]"
innerUnmacro (x:xs) = x:(innerUnmacro xs)

unmacro :: [BraceMod] -> String -> (String,[BraceMod]->[BraceMod])
unmacro _ "mod" = ("module",\s->[ModuleOpen, Module]++s)
unmacro _ "case" = ("case",\s->[CaseOpen, Case]++s)
unmacro _ "comb" = ("always_comb",id)
unmacro _ "i" = ("input",id)
unmacro _ "o" = ("output",id)
unmacro _ "l" = ("logic",id)
unmacro _ "r" = ("reg",id)
unmacro _ "ff" = ("always_ff @",id)
unmacro (ModuleOpen:_) "{" = (";",tail)
unmacro (CaseOpen:_) "{" = ("\n", tail)
unmacro _ "{" = ("begin",\s->[Default]++s)
unmacro (Module:_) "}" = ("endmodule",tail)
unmacro (Case:_) "}" = ("endcase", tail)
unmacro _ "}" = ("end",tail)
unmacro _ "->" = ("assign",id)
unmacro _ "]]" = (":0]", id)
unmacro _ "def>>" = ("default: ",id)
unmacro _ x = (x,id)

translate :: [BraceMod] -> [String] -> [String]
translate s [] = []
translate s (x:xs) = [(fst$unmacro s x)]++(translate ((snd$unmacro s x) s) xs)

removeSpaces :: [String] -> [String]
removeSpaces [] = []
removeSpaces (x:xs) | (and $ map (`elem`whitespace) x) = removeSpaces xs
                    | otherwise = [x]++(removeSpaces xs)

process :: String -> [String]
process code = translate [] $ removeSpaces $ wordsSpecial code

untab :: String -> String
untab [] = []
untab ('\t':'e':'n':'d':xs) = "end"++(untab xs)
untab (x:xs) = x:(untab xs)

fmt :: String -> [String] -> [String]
fmt t [] = []
fmt t (";":xs) = [";","\n",t]++(fmt t xs)
fmt t ("\n":xs) = ["\n",t]++(fmt t xs)
fmt t ("begin":xs) = [" begin","\n",t++"\t"]++(fmt (t++"\t") xs)
fmt t ("case":xs) = ["case "]++(fmt (t++"\t") xs)
fmt t ("module":xs) = ["module "]++(fmt (t++"\t") xs)
fmt t ("endmodule":xs) = ["endmodule","\n",tail t]++(fmt (tail t) xs)
fmt t ("end":xs) = ["end","\n",tail t]++(fmt (tail t) xs)
fmt t ("endcase":xs) = ["endcase","\n",tail t]++(fmt (tail t) xs)
fmt t (x:xs) | (((last x)`elem`(alphanumeric++"]<=")) && ((head$head xs)`elem`(alphanumeric++"[="))) = [x," "]++(fmt t xs)
             | otherwise = [x]++(fmt t xs)

main :: IO ()
main = do
    filename <- getArgs
    if (length filename == 0) || (length filename > 1) then (putStrLn "error! bad number of arguments!") else 
        do {
            handle <- openFile (head filename) ReadMode
        ;   output <- openFile (((reverse.dropWhile (/='.').reverse.head) filename)++"v") WriteMode
        ;   prg <- hGetContents handle
        ;   hPutStr output $ untab $ foldr (<>) "" $ fmt "" $ process prg
        ;   hClose handle
        ;   hClose output
        }
