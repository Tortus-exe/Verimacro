import System.IO
import System.Environment
import Control.Monad

data BraceMod = Default | Module

unmacro :: [BraceMod] -> String -> (String,[BraceMod]->[BraceMod])
unmacro _ "mod" = ("module",\s->[Module]++s)
unmacro _ "i" = ("input",id)
unmacro _ "l" = ("logic",id)
unmacro _ "ff" = ("always_ff @",id)
unmacro (Module:_) "{" = (";",id)
unmacro _ "{" = ("begin ",id)
unmacro (Module:_) "}" = ("endmodule",tail)
unmacro _ "}" = ("end",id)
unmacro _ x = (x,id)

translate :: [BraceMod] -> [String] -> [String]
translate s [] = []
translate s (x:xs) = [(fst$unmacro s x)]++(translate ((snd$unmacro s x) s) xs)

splitParens :: [String] -> [String]
splitParens (('(':xs):ks) = ["("]++[xs]++ks
splitParens (('{':xs):ks) = ["{"]++[xs]++ks
splitParens (x:xs) = [x]++xs
splitParens [] = []

process :: String -> String
process code = unwords $ translate [] $ splitParens $ words code

main :: IO ()
main = do
    filename <- getArgs
    if (length filename == 0) || (length filename > 1) then (putStrLn "error! bad number of arguments!") else 
        do {
            handle <- openFile (head filename) ReadMode
        ;   prg <- hGetContents handle
        ;   print $ process prg
        ;   hClose handle
        }
