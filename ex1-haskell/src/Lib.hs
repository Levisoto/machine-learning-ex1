module Lib where

-- import Statistics.LinearRegression
-- import qualified Data.Vector.Unboxed as U
import Data.Monoid
import Data.Decimal
import Data.Matrix
import Graphics.EasyPlot

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data LinReg m a = Single a 
                | Cons m a (LinReg m a)
                deriving (Show, Eq)

data Values = Values {xval :: Matrix Double , yval :: Matrix Double,  
  theta :: Matrix Double , alpha :: Double}
  deriving (Show,Eq)



tag::(Monoid m) => LinReg m (Values) -> m
tag (Single _) = mempty
tag (Cons m _ _) = m

compute :: LinReg [Double] Values -> LinReg [Double] Values
compute b@(Single a) = Cons (tag b) (newV a) b
compute b@(Cons m a val) = Cons ([funCost a]<>(tag val)) (newV a) b

newV :: Values -> Values
newV a@(Values xval yval theta alpha) = Values xval yval (funGrad a) alpha

takeLinReg:: Int -> LinReg [Double] Values -> LinReg [Double] Values 
takeLinReg 0 val = val
takeLinReg n val  
  | n>=0 = takeLinReg (n - 1) (compute val)
  | otherwise = val

functionY :: [(Double,Double)] -> (Double-> Double) -> [(Double,Double,Double)] -> IO Bool
functionY datas g data3d = do 
  plot X11 $ [
                    -- Function2D
                    -- [Title "Linear Regression", Color Red]
                    -- [Range 0 25] (g)
                    -- , Data2D
                    -- [Title "Data", Color Blue]
                    -- [Range 0 25] datas
                    -- , 
                    Data3D
                    [Title "theta vs J", Color Green]
                    [RangeY 0 25] data3d
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
