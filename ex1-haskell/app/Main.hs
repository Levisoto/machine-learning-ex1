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
     val = takeLinReg 1500 $ Single input
     (j,t) = unzip $ toPlot val
     p = zip [1500,1499..1] (j)
  in do
    writeFile "jvalues.txt" $ show $ head t
    plotData p
  -- putStrLn $ (show.funCost) input
  -- in functionY (takeFrom datas) (\ x -> b0 + b1*x)
 return()

-- toPlot:: [Double] -> Matrix Double -> [(Double,Double,Double)]
-- toPlot j theta = 
 -- if ans /= "y" then do
 --   putStrLn "not quitting"
 --   main
 -- else return ()
