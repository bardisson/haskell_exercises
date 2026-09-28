helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello, what is your name?"
  name <- getLine -- getLine returns IO String, <- allows us to use the IO String type as an ordinary String type
  let statement = helloPerson name
  putStrLn statement
