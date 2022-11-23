import System.IO
import System.Environment
import Control.Monad

data BraceMod = Default | Module | ModuleOpen

innerUnmacro :: String -> String
innerUnmacro [] = []
innerUnmacro (':':[]) = ":0]"
innerUnmacro (x:xs) = x:(innerUnmacro xs)

unmacro :: [BraceMod] -> String -> (String,[BraceMod]->[BraceMod])
unmacro _ "mod" = ("module",\s->[ModuleOpen, Module]++s)
unmacro _ "i" = ("input",id)
unmacro _ "o" = ("output",id)
unmacro _ "l" = ("logic",id)
unmacro _ "ff" = ("always_ff @",id)
unmacro (ModuleOpen:_) "{" = (";",tail)
unmacro _ "{" = ("begin",\s->[Default]++s)
unmacro (Module:_) "}" = ("endmodule",tail)
unmacro _ "}" = ("end",tail)
unmacro _ "->" = ("assign",id)
unmacro _ x = (innerUnmacro x,id)

translate :: [BraceMod] -> [String] -> [String]
translate s [] = []
translate s (x:xs) = [(fst$unmacro s x)]++(translate ((snd$unmacro s x) s) xs)

splitParens :: [String] -> [String]
splitParens (('(':xs):ks) = ["("]++[xs]++(splitParens ks)
splitParens (('{':xs):ks) = ["{"]++[xs]++(splitParens ks)
splitParens (x:xs) = [x]++(splitParens xs)
splitParens [] = []
--splitParens (('[':xs):ks) = ["["]++[xs]++(splitParens ks)

process :: String -> String
process code = unwords $ translate [] $ splitParens $ words code

--  initial tabbing -> string to prettify -> output
prettify :: String -> String -> String
prettify t [] = []
prettify t (';':xs) = ";\n"++t++(prettify t xs)
prettify t ('b':'e':'g':'i':'n':xs) = "begin\n"++"\t"++t++(prettify ("\t"++t) xs)
prettify t ('e':'n':'d':'m':'o':'d':'u':'l':'e':xs) = "endmodule\n"++(prettify (drop 1 t) xs)
prettify t ('e':'n':'d':xs) = "end\n"++(drop 1 t)++(prettify (drop 1 t) xs)
prettify t ('m':'o':'d':'u':'l':'e':xs) = "module"++(prettify ("\t"++t) xs)
prettify t (x:xs) = x:(prettify t xs)

unspace :: String -> String
unspace [] = []
unspace (x:' ':'(':xs) = x:'(':(unspace xs)
unspace ('(':' ':xs) = '(':(unspace xs)
unspace (' ':' ':xs) = ' ':(unspace xs)
unspace (' ':';':xs) = ';':(unspace xs)
unspace (';':' ':xs) = ';':(unspace xs)
unspace (x:xs) = x:(unspace xs)

untab :: String -> String
untab [] = []
untab ('\t':'e':'n':'d':xs) = "end"++(untab xs)
untab ('\t':' ':'e':'n':'d':xs) = "end"++(untab xs)
untab (x:xs) = x:(untab xs)

main :: IO ()
main = do
    filename <- getArgs
    if (length filename == 0) || (length filename > 1) then (putStrLn "error! bad number of arguments!") else 
        do {
            handle <- openFile (head filename) ReadMode
        ;   prg <- hGetContents handle
        ;   putStrLn$ untab $ prettify "" $ unspace . unspace $ process prg
        ;   hClose handle
        }
