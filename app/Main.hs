module Main where

import MyParser
import qualified Text.Parsec as TP

main :: IO ()
main = do { putStrLn "Hello, Haskell! ******************************************"
          ; res <- parseFuncF manyFuncTokD "input_prog.txt"
          ; case res of
              Left e -> putStrLn ("__ERROR *****\n" ++ show e)
              Right ok -> putStrLn ("++OK +++++++++\n" ++ show ok)
          ; state <- parseFuncFS manyFuncTokD "input_prog.txt"
          ; putStrLn ("STATE *****\n" ++ show state)
          }
