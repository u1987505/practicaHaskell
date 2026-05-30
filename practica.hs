module Main where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

-- A very small interactive calculator.
-- Input examples:
--   2 + 3
--   4.5 * 6
--   10 / 2
--   quit

main :: IO ()
main = do
  putStrLn "Basic Haskell Calculator. Enter expressions like: 2 + 3 or 4.5 * 6. Type 'quit' to exit."
  loop

loop :: IO ()
loop = do
  putStr "> "
  line <- getLine
  case words line of
    ["quit"] -> putStrLn "Goodbye."
    [a,op,b] -> do
      let ma = readMaybe a :: Maybe Double
          mb = readMaybe b :: Maybe Double
      case (ma, mb) of
        (Just x, Just y) ->
          case calc op x y of
            Just r  -> print r >> loop
            Nothing -> putStrLn "Unknown operator. Use + - * /" >> loop
        _ -> putStrLn "Could not parse numbers." >> loop
    _ -> putStrLn "Invalid input. Use format: <num> <op> <num>" >> loop

calc :: String -> Double -> Double -> Maybe Double
calc "+" x y = Just (x + y)
calc "-" x y = Just (x - y)
calc "*" x y = Just (x * y)
calc "/" _ 0 = Nothing
calc "/" x y = Just (x / y)
calc _ _ _ = Nothing
