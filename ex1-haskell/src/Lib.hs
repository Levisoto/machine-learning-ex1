module Lib where

import Data.Monoid
import Data.Decimal
import Data.Matrix
import Graphics.EasyPlot

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data LinReg j t a = Single a 
                  | Cons j t a (LinReg j t a)
                deriving (Show, Eq)

data Values = Values {xval :: Matrix Double , yval :: Matrix Double,  
  theta :: Matrix Double , alpha :: Double}
  deriving (Show,Eq)

compute :: LinReg Double (Matrix Double) Values -> LinReg Double (Matrix Double) Values
compute b@(Single a) = Cons (funCost nVal) (theta nVal) nVal b
  where
    nVal = newV a
compute b@(Cons j t a val) = Cons (funCost nVal) (theta nVal) nVal b
  where
    nVal = newV a

newV :: Values -> Values
newV a@(Values xval yval theta alpha) = Values xval yval (funGrad a) alpha

toPlot :: LinReg Double (Matrix Double) Values -> [(Double,Matrix Double)]
toPlot (Single a) = []
toPlot (Cons j t a val) = [(j,t)] ++ toPlot val

takeLinReg :: Int -> LinReg Double (Matrix Double) Values -> LinReg Double (Matrix Double) Values
takeLinReg 0 val = val
takeLinReg n val  
  | n>=0 = takeLinReg (n - 1) (compute val)
  | otherwise = val

plotData :: [(Double,Double)] -> IO Bool
plotData datas = do 
  plot X11 $ [
                    -- Function2D
                    -- [Title "Linear Regression", Color Red]
                    -- [Range 0 25] (g)
                     Data2D
                    [Title "Data", Color Blue, Style Lines]
                    [Range 0 25] datas
                    -- -- , 
                    -- Data3D
                    -- [Title "theta vs J", Color Green]
                    -- [RangeY 0 25] data3d
                 ]



takeFrom :: String -> [(Double,Double)]
takeFrom = map (\(x:y:_)->(x,y)).map divByComa.lines

takeFrom' :: String -> [(Double,[Double])]
takeFrom' = map (\(x:xs)-> (x,reverse xs)).map (reverse.divByComa).lines

divByComa :: String -> [Double]
divByComa = map read.words.map rep
  where
    rep ',' = ' '
    rep c = c

toValues:: [[Double]] -> [Double] -> [Double] -> Double -> Values
toValues x y theta alpha = Values x' y' theta' alpha
  where
    m      = length x
    x'     = ((fromLists.take m.repeat) [1]) <|> fromLists x
    y'     = fromList m 1 y
    theta' = fromList (length theta) 1 theta

funGrad :: Values -> Matrix Double
funGrad (Values x y theta alpha) = final
    where
      m      = nrows x
      sol    = elementwise (\h g -> h-g)(multStd2 x theta) y
      sol'   = multStd2 (transpose x) sol
      final = elementwise (\h g -> h-g) theta (scaleMatrix (alpha/(fromIntegral m)) sol')


funCost:: Values -> Double
funCost (Values x y theta alpha) = (0.5/(fromIntegral m))*(getSum $ foldMap (\h->Sum h) sol')
    where
      m      = nrows x
      sol    = elementwise (\h g -> h-g)(multStd2 x theta) y
      sol'   = fmap (\x -> x^2) sol
