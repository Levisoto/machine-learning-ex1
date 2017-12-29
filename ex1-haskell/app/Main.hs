module Main where

import Lib

main :: IO ()
main = do
  -- putStrLn "quit? y/n"
  -- ans <- getLine
  -- when (ans /= "y") $ do
  --   putStrLn "not quiting"
  --   main
 -- putStrLn "quit the program? y/n"
 -- ans <- getLine
 datas <- readFile "ex1data1.txt"
 functionY (takeFrom datas)
 return()
 -- if ans /= "y" then do
 --   putStrLn "not quitting"
 --   main
 -- else return ()
