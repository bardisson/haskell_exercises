import Text.XHtml (variable)
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
 
isEvenInc n = ifEven inc n
isEvenDouble n = isEven double n
isEvenSquare n = isEven square n

-- important note: in Haskell, function evaluation ALWAYS has highest precedence in an evaluation
