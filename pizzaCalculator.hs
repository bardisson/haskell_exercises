import GHC.Internal.RTS.Flags (TraceFlags(user))
import Data.Binary.Put (putShortByteString)
-- create function to calculate area given diameter
areaGivenDiameter :: Double -> Double
areaGivenDiameter diameter = pi * ((diameter/2)^2)

type Pizza = (Double, Double) 

-- Calculate the cost per inch
costPerInch :: Pizza -> Double
costPerInch (size, cost) =  cost / areaGivenDiameter(size)

-- Now compare two pizzas
comparePizzas :: Pizza -> Pizza -> Pizza -- takes in two Pizza type class and returns Pizza type class
comparePizzas p1 p2 = 
  if costP1 < costP2
  then p1
  else p2
  where 
    costP1 = costPerInch p1
    costP2 = costPerInch p2

-- describe the output pizza to the user
describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza is cheaper at " ++ show costSqInch ++ " per square inch."
  where costSqInch = costPerInch (size, cost)

-- now bring this all together with an IO action
main :: IO ()
main = do
  putStrLn "What is the size of pizza 1?"
  size_p1 <- getLine
  putStrLn "What is the cost of pizza 1?"
  cost_p1 <- getLine
  putStrLn "What is the size of pizza 2?"
  size_p2 <- getLine
  putStrLn "What is the cost of pizza 2?"
  cost_p2 <- getLine
  let pizza1 = (read size_p1, read cost_p1) -- recall read String -> a
  let pizza2 = (read size_p2, read cost_p2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

