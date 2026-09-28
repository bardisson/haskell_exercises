import Text.XHtml (variable, name)
import Data.List
import GHC.Exts.Heap (allClosures)
import Distribution.Compat.Graph (closure)
-- in lambda calculus you represent everything as functions; even all integers can be represented by functions

-- in Haskell all functions must have an argument AND return a value
simple x = x

-- referential transparency
x = 2 -- assign a value to a variable
-- x = 3 -- this is illegal

{- example of a poorly written function (repeat calculation, needing to reason on each line)
calcChange owed given = if given - owed > 0
                        then given - owed
                        else 0
-}
calcChange owed given = if change > 0 
                        then change
                        else 0
  where change = given - owed

-- LESSON 3 --------------------------------
-- rewrite the simple function as a lambda function:
simple2 = \x -> x

-- rewriting the where clause, right now we only know of the where clause as the way to store variables inside a function
sumSquareOrSquareSum x y = if sumSquare > squareSum
                            then sumSquare
                            else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x+y)^2

-- if we didn't use where we would represent the body of this function as a lambda function and pass in the sumSquare and squareSum operations as arguements to the lambda function
sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum) (x^2 + y^2) ((x+y)^2)

-- There "where" statement is powerful, but currently wrapped up in a function, how can we extract to use this concept elsewhere?
-- Enter the "let" expression
sumSquareOrSquareSum x y =  let   sumSquare = (x^2 + y^2)
                                  squareSum = (x+y)^2
                            in
                              if sumSquare > squareSum
                              then sumSquare
                              else squareSum
-- use of the where or let style is up to the author and purely a matter of preference                            

-- lambda functions are powerful (where/let) enable you to make a focused scope on the fly so that variables can be redefined within this "lexical" scope

-- LESSON 4 --------------------------------------
-- First class functions: functions are not different from data, functions can be used as arguments and returns as values from other functions

-- say you have this function
isEvenInc n = if even n
            then n+1
            else n
-- then say later you need to modify the behavior of the increment...
isEvenDouble n = if even n
  then n*2
  else n
isEvenSquare n = if even n 
  then n^2
  else n

-- we can rewrite this using functional composition and the fact that functions are first class via
inc n = n+1
double n = n*2
square n = n^2

isEven myFunction x = if even x
  then myFunction x
  else x
 
--isEvenInc n = ifEven inc n
--isEvenDouble n = isEven double n
--isEvenSquare n = isEven square n

-- important note: in Haskell, function evaluation ALWAYS has highest precedence in an evaluation

-- ############################################# 9/18/26
-- lets look at sorting
-- import Data.List (imported at top)
names = [("Ian", "Curtis"),
         ("Bernard", "Sumner"),
         ("Peter", "Hook"),
         ("Stephan", "Morris")]
-- now we can call "sort names" from ghci -- but what if we wanted to sort by a specific element?

-- function to take the second element of each name tuple in the list and compare
compareLastNames name1 name2 = 
  let lastName1 = snd name1
      lastName2 = snd name2
  in
    if lastName1 > lastName2 then GT
    else if  lastName1 < lastName2 then LT
    else EQ

-- now when would be a case when we would like to return not just a value, but a function??

--addressLetter name location = nameText ++ " - " ++ location
--  where nameText = (fst name) ++ " " ++ (snd name)

{- this isn't great...
 usage: addressLetter ("Bob", "Smith") "PO Box 1234 - San Francisco, CA, 94111"
-}

-- say some additional constraints are introduced, modelled by the following functions:
sfOffice name = if lastName < "L"
  then nameText ++ "PO Box 1234 - San Francisco, CA 94111"
  else nameText ++ "PO Box 1010 - San Francisco, CA 94111"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = (snd name)

-- now how to build and use this in a larger function?
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name)) -- return generic solution for wildcard

-- now we can redefine the addressLetter function as follows
addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location
-- usage: addressLetter ("Bob", "Smith") "ny"


-- LESSON 5 --------------------------------------
-- Closures

-- we don't want to repeat any patterns; recall how ifEvenInc/Double/Square was still repeating the call on ifEven
genIsEven f = (\x -> isEven f x) -- we have captured function f inside a lambda function, this is a CLOSURE
-- | test: genIsEven inc 2 -> 3

-- URL builder as a mode advanced application of Closures
getRequestURL host apiKey resource id  = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey
-- | usage: getRequestURL "http://example.com" "1337hAsk3ll" "book" "1234"

-- now add a generator function to reduce the repetition of writing out the URL everytime we make this call (and use in a closure)
genHostRequestBuilder host = (\apiKey resource id -> getRequestURL host apiKey resource id)
exampleUrlBuilder = genHostRequestBuilder "http://example.com" -- | naming this anon function exampleUrlBuilder captures it for use elsewhere
-- | usage: exampleUrlBuilder "1337hAsk3ll" "book" "1234"
-- now a step further
genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll" -- use myExampleUrlBuilder as an anon capture of the genApiRequestBuilder closure
-- | test: myExampleUrlBuilder "book" "1234" -> "http://example.com/book/1234?token=1337hAsk3ll"

-- Partial application is Haskell's solution to dealing with closures that are hard to read with lambda notation and many args
add4 a b c d = a + b + c + d
-- | test: mystery add4 3 (this creates a brand new function "mystery" that is waiting for us to provide the remaining 3 arguments, partial application!)
-- Much of what we just wrote for the URL builder can be written much more easily when using partial application

-- Because of partial application, we should order function arguments from Most to Least general

-- Say now we inherited some code and need to fix the order of the arguments of some previously defined stuff?
flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)
addressLetterV2 = flipBinaryArgs addressLetter
addressLetterNY = addressLetterV2 "ny" -- notice now we pass location first, then name, since we have flipped the input args through use of our partial appliation of a closure


-- LESSON 6 --------------------------------------
-- Lists

-- LESSON 21 --------------------------------------
-- IO
-- See helloPerson.hs
