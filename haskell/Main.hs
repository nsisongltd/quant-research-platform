{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Time (parseTimeM, defaultTimeLocale)
import System.Environment (getArgs)

data Trade = Trade
  { tradeId    :: !Int
  , symbol     :: !String
  , timestamp  :: !String
  , price      :: !Double
  , quantity   :: !Int
  , side       :: !String
  } deriving (Show)

instance Csv.FromNamedRecord Trade where
  parseNamedRecord r =
    Trade <$> r Csv..: "id"
          <*> r Csv..: "symbol"
          <*> r Csv..: "timestamp"
          <*> r Csv..: "price"
          <*> r Csv..: "quantity"
          <*> r Csv..: "side"

validate :: Trade -> [String]
validate t =
  [ "invalid price"    | price t <= 0 ] ++
  [ "invalid quantity" | quantity t <= 0 ] ++
  [ "invalid side"     | side t `notElem` ["BUY", "SELL"]] ++
  [ "invalid timestamp"| not (validTimestamp (timestamp t)) ]

validTimestamp :: String -> Bool
validTimestamp ts =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" ts of
    Just _  -> True
    Nothing -> False

main :: IO ()
main = do
  args <- getArgs
  let filePath = case args of
        [] -> "python/data/trades.csv"
        [path] -> path
        _ -> error "usage: validator [trades.csv]"
  
  csvData <- BL.readFile filePath
  case Csv.decodeByName csvData of
    Left err -> putStrLn ("csv parse error: " ++ err)
    Right (_, v) -> do
      putStrLn $ "validating trades from: " ++ filePath
      putStrLn "=" ++ replicate 50 '='
      
      let errors = V.toList $ V.concatMap (V.fromList . validate) v
          totalTrades = V.length v
      
      putStrLn $ "total trades processed: " ++ show totalTrades
      putStrLn $ "valid trades: " ++ show (totalTrades - length errors)
      putStrLn $ "invalid trades: " ++ show (length errors)
      
      if null errors
        then putStrLn "\nall trades are valid!"
        else do
          putStrLn "\nvalidation errors found:"
          mapM_ (\err -> putStrLn $ "  " ++ err) errors
