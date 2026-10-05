{-|
 Found some pretty obvious optimizations during this problem. Also didn't realize that we could use reverse transpose to do the "ring peel" technique. i.e.:  

  A ring peel is "take the top row, rotate the rest counterclockwise, repeat":
  
  import Data.List (transpose)
  
  snail :: [[a]] -> [a]
  snail []       = []
  snail (x:xs)   = x ++ snail (reverse (transpose xs))
  
  transpose + reverse is the counterclockwise rotation, so the next top row is the old right column
-}


import Debug.Trace (trace)
import Data.List (transpose)
-- https://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1

testList = [[1,2,3], [4,5,6], [7,8,9]] -- this is not an Array, but a list of lists; not using Data.Array 
testList2 = [[1,2,3,1], [4,5,6,4], [7,8,9,7], [7,8,9,7]] -- this is not an Array, but a list of lists; not using Data.Array 
 

-- #####################################
-- #################### Helper Functions
-- #####################################
getBounds :: [[Int]] -> (Int, Int)
getBounds array = (maxR, maxC) 
  where
  maxR = length (filter (not . null) array) -- return the number of non empty lists inside the outer list
  maxC = length (array !! 0)                -- get the length of one of the inner lists

printBounds :: (Int, Int) -> IO ()
printBounds (maxR, maxC) = 
  putStrLn$ "Rows: " ++ show maxR ++ " Columns: " ++ show maxC

{-|
 Return the first row of the 2d array if not null.
-}
extractRow :: [[Int]] -> Int -> [Int]
extractRow array ridx = if (not . null) row then row else []
  where row = array !! ridx

{-| 
 Returns every Nth value from a list.
 Recursive implementaton for efficiency
 Note: this doesn't work because it doesn't include the extracted element 
 in each successive recurse of drop - results in off-by-inc as the recursion cycles
-}
everyN :: Int -> [a] -> [a]
everyN n xs = case drop (n - 1) xs of
  []      -> []
  (y:ys)  -> y : everyN n ys

{-|
 Returns every n-th value in list, starting from offset
 Very inefficient since we need to reevaluate pointer index (w/ !!) on every operation.
-}
-- TODO: This won't evaluate online because it is too slow O(n^3) since xs !! i is O(i) over an array of length n, each column costs O(n^2) with O(n) for "length xs"
everyNOffset :: Int -> Int -> [Int] -> [Int]
everyNOffset n offs xs
  | (n - offs) >= 0 = [xs !! i | i <- [offs + (1*n), offs + (2*n) .. (length xs-1)]]
  | otherwise = error "  n - offset less than 0 "

eleA2B :: Int -> Int -> [Int] -> [Int]
eleA2B start stop array = take (stop - start + 1)(drop start array)

{-|
 Flatten the 2D list using an "unbind" with the identity function.
 I.e.: id flip(>>=) input_list
-}
-- TODO: This is really just "concat"
flatten2D :: [[Int]] -> [Int]
flatten2D list = id =<< list

buildRowFwd :: Int -> Int -> [Int] -> [Int]
buildRowFwd currR n inputList = eleA2B start stop inputList
  where
    start = ( currR *      n ) + currR            -- start at specfic row offset
    stop  = ((currR + 1) * n ) - currR - offset
    offset = if even n then 1 else 2

buildColFwd :: Int -> Int -> [Int] -> [Int]
buildColFwd currR n inputList = init $ everyNOffset n offset inputList
  where   
  offset = (-n) + (n - 1 - currR)

buildRowRev :: Int -> Int -> [Int] -> [Int]
buildRowRev currR n inputList = init $ reverse $ eleA2B start stop inputList
  where
    row   = n - 1 - currR  -- extra decrement due to size being 1 indexed
    start = (n * row)
    stop  = start + n - (currR + 1)

buildColRev :: Int -> Int -> [Int] -> [Int]
buildColRev currR n inputList = init $ reverse $ everyNOffset n offset inputList
  where
    offset = currR - n

checkComplete :: Int -> Int -> Bool
checkComplete currR n =
  -- trace takes a message string and a return expression
  --trace ("Checking complete with currR=" ++ show currR ++ " and n=" ++ show n) $
  if even n 
    then evenCheck currR n
    else oddCheck currR n

-- TODO: This doesn't guard against N < 3 which can cause this to never fire
evenCheck :: Int -> Int -> Bool
evenCheck currR n = currR == (n - 3) -- simplified from (n-1) - 2

oddCheck :: Int -> Int -> Bool
oddCheck currR n = currR == (n - 2) -- simplified from (n-1) - 1


-- #####################################
-- ################ Solve Attempts
-- #####################################

runCoilLoop :: Int -> Int -> [Int] -> [Int] -> [Int]
runCoilLoop currR n matrix acc
  | checkComplete currR n = finalAcc -- return current accumulated list
  | otherwise             = runCoilLoop nextR n matrix updatedAcc
  where
  -- TODO: The "acc + recurseOut" is quadratic since it rewalks the accumulator every iteration. We shouldn't accumulate forward, but instead let laziness evaluate it 
    recurseOut =     buildRowFwd currR n matrix
                  ++ buildColFwd currR n matrix
                  ++ buildRowRev currR n matrix
                  ++ buildColRev currR n matrix
    updatedAcc = acc ++ recurseOut
    nextR = currR + 1

    finalAcc
      | even n    = acc ++ buildRowFwd currR n matrix
                  ++ buildRowRev currR n matrix
      | otherwise = acc ++ [matrix !! ( ( currR * n ) + 1 )]

method1 :: IO ()
method1 = do
  let dut     = testList
  let matrix  = flatten2D dut
  let n       = snd $ getBounds dut
  printBounds$ getBounds dut
  let snakePath = runCoilLoop 0 n matrix []
  print snakePath

snailProper :: [[a]] -> [a]
snailProper [] = [] -- define a function for the empty list case
-- use the cons operator for reverse pattern match; where x is the first row and xs is the remaining rows of the matrix
snailProper (x:xs) = x ++ snailProper (reverse(transpose xs))
-- ^ as the transpose extracts the line, it always transposes on a matrix with one less row; resulting in eventual termination

method2 :: IO()
method2 = do
  print(snailProper testList :: [Int])
  print(snailProper testList2 :: [Int])

-- #####################################
-- ############ Kata Submission Call
-- #####################################
--snail :: [[Int]] -> [Int]
--snail array = runCoilLoop 0 (snd $ getBoundsarray ) (flatten2D array) []
