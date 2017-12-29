module Lib where

import Statistics.LinearRegression
import qualified Data.Vector.Unboxed as U
import Data.Decimal
import Data.Matrix
import Graphics.EasyPlot

someFunc :: IO ()
someFunc = putStrLn "someFunc"
--xs :: [Double]
--xs = [25.47,67.80,20.65,22.50,30.40,55.30,22.74] ++
--         [29.11,31.15,40.97,58.89,64.85,74.89,96.14] ++
--         [65.37,80.23,95.80,92.71,98.16,89.77]

--ys :: [Double]
--ys = [390.50,632.70,449.36,501.81,621.72,684.62,685.53] ++
--         [701.76,868.71,995.69,893.50,1240.48,1393.55] ++
--         [1826.70,2145.32,2295.27,2522.79,2931.69] ++
--         [2836.20,2456.30]

---- Values from a list
----
--varIndep  = U.fromList xs
--varDepend = U.fromList ys

---- computing theta0 and theta1 : Y = t0 + b1.X

--(b0, b1) = linearRegression varIndep varDepend

--b0n = realFracToDecimal 3 b0
--b1n = realFracToDecimal 3 b1

--points = zip xs ys

--f x = b0 + b1*x
--g x = ((*b0).sin.(/b1)) x

functionY datas= do 
  plot X11 $ [
                    -- Function2D
                    -- [Title "Linear Regression", Color Red]
                    -- [Range 0 25] (g)
                     Data2D
                    [Title "Data", Color Blue]
                    [Range 0 25] datas
                    -- , Function2D
                    -- [Title "Linear Regression", Color Red]
                    -- [Range 0 25] (f)
                 ]



takeFrom :: String -> [(Double,Double)]
takeFrom = map divByComa.lines

divByComa :: String -> (Double, Double)
divByComa val = ((read.ins) val, (read.reverse.ins.reverse) val)
  where 
    ins = takeWhile (/= ',')
