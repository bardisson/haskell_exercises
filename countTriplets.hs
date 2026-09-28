import Control.Monad (forM_)
import Data.IORef

getInts :: IO [Int]
getInts = map read . words <$> getLine
-- <$> is an alias for fmap, which allows us to map a function across a container/context like Maybe, IO, or a List

-- first attempt, works, but is not idiomatic (avoiding mutable states, IO references)
compareTriplets :: IO()
compareTriplets = do
  scoreA <- newIORef (0 :: Int)
  scoreB <- newIORef (0 :: Int)

  a <- getInts
  b <- getInts

  forM_ (zip a b) $ \(valA, valB) -> do
    case compare valA valB of
      LT -> modifyIORef scoreB succ
      GT -> modifyIORef scoreA succ 
      EQ -> pure()

  finalA <- readIORef scoreA
  finalB <- readIORef scoreB
  putStrLn $ show finalA ++ " " ++ show finalB

-- second attempt, instead of mutating variables in a loop we can process the list using zip and a pure foldl
compareTripletsPure :: [Int] -> [Int] -> (Int, Int)
compareTripletsPure a b = foldl' updateScore (0, 0) (zip a b) -- foldl is lazy evaluated, which can cause large thunk to sit in memory or may crash on long lists
  where
    updateScore (scoreA, scoreB) (valA, valB) = case compare valA valB of
      GT -> (succ scoreA, scoreB)
      LT -> (scoreA, succ scoreB)
      EQ -> (scoreA, scoreB)

main :: IO ()
main = do
 a <- getInts
 b <- getInts
 let (finalA, finalB) = compareTripletsPure a b
 putStrLn $ show finalA ++ " " ++ show finalB

-- third attempt, found online, ultra concise approach using filter
mainConcise :: IO ()
mainConcise = do
  a <- getLine
  b <- getLine
  let 
    zippedList = zip a b
    {- 
    This works becuase we only need to count how many times an element is greater than the other in a list.
    Filter applies the predicate to the list and returns the elementst that satisfy that predicate. Length then counts them up
    which works becuase the lists are of equal length. Additional guards would be needed to ensure that this is checked.
    -}
    scoreA = length $ filter (\(x, y) -> x > y) zippedList
    scoreB = length $ filter (\(x, y) -> x < y) zippedList
  putStrLn $ show scoreA ++ " " ++ show scoreB
