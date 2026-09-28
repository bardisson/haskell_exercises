import System.Environment

main :: IO ()
main = do
  args <- getArgs
  -- map only works on ordinary lists
  -- mapM traverses across the IO Monad context but map always returns a list and main is of type IO ()
  -- mapM_ traverses mondaic actions and discards the result mapM_
  mapM_ putStrLn args
  
  let comPort = if length args > 0
                then read (head args)
                else 0
  
