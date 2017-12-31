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
 -- functionY (takeFrom datas)
 let (y,x) = unzip $ takeFrom' datas
     input = toValues x y [0,0] 0.01
     (Cons j value _)= takeLinReg 15000 $ Single input
     -- [b0,b1] = theta value
  in writeFile "jvalues.txt" $ show (theta value)
  -- putStrLn $ (show.funCost) input
  -- in functionY (takeFrom datas) (\ x -> b0 + b1*x)
 return()
 -- if ans /= "y" then do
 --   putStrLn "not quitting"
 --   main
 -- else return ()
