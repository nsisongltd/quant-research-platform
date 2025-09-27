{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Time (parseTimeM, defaultTimeLocale)

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
  csvData <- BL.readFile "trades.csv"
  case Csv.decodeByName csvData of
    Left err -> putStrLn ("csv parse error: " ++ err)
    Right (_, v) -> V.forM_ v $ \trade ->
      case validate trade of
        [] -> return ()
        errs -> putStrLn $ "trade " ++ show (tradeId trade) ++ ": " ++ unwords errs
