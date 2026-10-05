import Debug.Trace (trace)
-- https://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1
-- Given an n x n array, return the array elements arranged from outermost elements to the middle element, traveling clockwise.
-- 
-- array = [[1,2,3],
--          [4,5,6],
--          [7,8,9]]
-- snail(array) #=> [1,2,3,6,9,8,7,4,5]
-- 
-- This list is square and not empty -- so no need to check this

testList = [[1,2,3], [4,5,6], [7,8,9]] -- this is not an Array, but a list of lists; not using Data.Array 
testList2 = [[1,2,3,1], [4,5,6,4], [7,8,9,7], [7,8,9,7]] -- this is not an Array, but a list of lists; not using Data.Array 
 
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
 Very inefficient since we need to reevaluate pointer index (w/ !!) on every operation
-}
everyNOffset :: Int -> Int -> [Int] -> [Int]
everyNOffset n offs xs
  | (n - offs) >= 0 = [xs !! i | i <- [offs + (1*n), offs + (2*n) .. (length xs-1)]]
  | otherwise = error "  n - offset less than 0 "

eleA2B :: Int -> Int -> [Int] -> [Int]
eleA2B start stop array = take (stop - start + 1)(drop start array)

-- This first approach was trying to build the list without flattening the input list -- changing approach
--main :: IO ()
--main = do
--  let (maxR, maxC) = getBounds testList
--  let outList = eleA2B 0 maxC (extractRow testList 0)
--  printBounds(maxR, maxC)
--  print (outList :: [Int])

{-|
 Flatten the 2D list using an "unbind" with the identity function.
 I.e.: id flip(>>=) input_list
-}
flatten2D :: [[Int]] -> [Int]
flatten2D list = id =<< list

--snail :: [Int] -> Int -> [Int]  -- input list, n (n x n square), outputs ordered list
--snail list n = go currR currC
--  where
  -- iter 0
  -- row currR;         currC to (col n-1)
  -- col n - currC;     currR to (row n-1)
  -- row n - currR;     col n - currC to currC + 1 && reverse()
  -- col currC;         row n - currR to (currR + 1) && reverse()
  -- currR & currC + 1
  -- iter 1
  -- row currR;         currC to (col n-1)
  -- col n - currC;     currR to (row n-1)
  -- row n - currR;     

buildRowFwd :: Int -> Int -> Int -> [Int] -> [Int]
buildRowFwd currR currC n inputList = eleA2B start stop inputList
  where
    start = ( currR *      n ) + currR            -- start at specfic row offset
    stop  = ((currR + 1) * n ) - currR - offset
    offset = if even n then 1 else 2

buildColFwd :: Int -> Int -> Int -> [Int] -> [Int]
buildColFwd currR currC n inputList = init $ everyNOffset n offset inputList
  where   
  offset = (-n) + (n - 1 - currR)

buildRowRev :: Int -> Int -> Int -> [Int] -> [Int]
buildRowRev currR currC n inputList = init $ reverse $ eleA2B start stop inputList
  where
    row   = n - 1 - currR  -- extra decrement due to size being 1 indexed
    start = (n * row)
    stop  = start + n - (currC + 1)

buildColRev :: Int -> Int -> Int -> [Int] -> [Int]
buildColRev currR currC n inputList = init $ reverse $ everyNOffset n offset inputList
  where
    offset = currC - n

runCoilLoop :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
runCoilLoop currR currC n matrix acc
  | checkComplete currR n = finalAcc -- return current accumulated list
  | otherwise             = runCoilLoop nextR nextC n matrix updatedAcc

  where
    recurseOut =     buildRowFwd currR currC n matrix
                  ++ buildColFwd currR currC n matrix
                  ++ buildRowRev currR currC n matrix
                  ++ buildColRev currR currC n matrix

    updatedAcc = acc ++ recurseOut
    nextR = currR + 1
    nextC = currC + 1

    finalAcc
      | even n    = acc ++ buildRowFwd currR currC n matrix
--                  ++ buildColFwd currR currC n matrix
                  ++ buildRowRev currR currC n matrix
      | otherwise = acc ++ [matrix !! ( ( currR * n ) + 1 )]
  
checkComplete :: Int -> Int -> Bool
checkComplete currR n =
  -- trace takes a message string and a return expression
  --trace ("Checking complete with currR=" ++ show currR ++ " and n=" ++ show n) $
  if even n 
    then evenCheck currR n
    else oddCheck currR n

evenCheck :: Int -> Int -> Bool
evenCheck currR n = currR == (n - 3) -- simplified from (n-1) - 2

oddCheck :: Int -> Int -> Bool
oddCheck currR n = currR == (n - 2) -- simplified from (n-1) - 1

main :: IO ()
main = do

  let dut     = testList
  let matrix  = flatten2D dut
  let n       = snd $ getBounds dut
  printBounds$ getBounds dut
  
  let snakePath = runCoilLoop 0 0 n matrix []
  print snakePath

snail :: [[Int]] -> [Int]
snail array = runCoilLoop 0 0 (snd $ getBounds array ) (flatten2D array) []
