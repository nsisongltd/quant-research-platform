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

data Portfolio = Portfolio
  { cash      :: !Double
  , positions :: ![(String, Int)]
  } deriving (Show)

data RiskConfig = RiskConfig
  { maxPositionSize :: !Double
  , maxPortfolioExposure :: !Double
  , initialCapital :: !Double
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

validateRisk :: Trade -> Portfolio -> RiskConfig -> [String]
validateRisk trade portfolio config =
  let tradeValue = fromIntegral (quantity trade) * price trade
      currentPosition = lookup (symbol trade) (positions portfolio)
      newPosition = case (side trade, currentPosition) of
        ("BUY", Just pos) -> pos + quantity trade
        ("BUY", Nothing)  -> quantity trade
        ("SELL", Just pos) -> pos - quantity trade
        ("SELL", Nothing)  -> -quantity trade
  in
  [ "position size too large" | tradeValue > maxPositionSize config ] ++
  [ "insufficient cash for buy" | side trade == "BUY" && tradeValue > cash portfolio ] ++
  [ "insufficient position for sell" | side trade == "SELL" && maybe 0 id currentPosition < quantity trade ] ++
  [ "negative position not allowed" | newPosition < 0 ]

validTimestamp :: String -> Bool
validTimestamp ts =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" ts of
    Just _  -> True
    Nothing -> False

updatePortfolio :: Trade -> Portfolio -> Portfolio
updatePortfolio trade portfolio =
  let tradeValue = fromIntegral (quantity trade) * price trade
      currentPosition = lookup (symbol trade) (positions portfolio)
      newPosition = case (side trade, currentPosition) of
        ("BUY", Just pos) -> pos + quantity trade
        ("BUY", Nothing)  -> quantity trade
        ("SELL", Just pos) -> pos - quantity trade
        ("SELL", Nothing)  -> -quantity trade
      newCash = case side trade of
        "BUY"  -> cash portfolio - tradeValue
        "SELL" -> cash portfolio + tradeValue
      newPositions = case newPosition of
        0 -> filter ((/= symbol trade) . fst) (positions portfolio)
        _ -> (symbol trade, newPosition) : filter ((/= symbol trade) . fst) (positions portfolio)
  in Portfolio newCash newPositions

defaultRiskConfig :: RiskConfig
defaultRiskConfig = RiskConfig
  { maxPositionSize = 5000.0
  , maxPortfolioExposure = 0.1
  , initialCapital = 10000.0
  }

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
      
      let trades = V.toList v
          totalTrades = length trades
          initialPortfolio = Portfolio (initialCapital defaultRiskConfig) []
          (finalPortfolio, riskErrors) = foldl validateTradeSequence (initialPortfolio, []) trades
          basicErrors = concatMap validate trades
          allErrors = basicErrors ++ riskErrors
      
      putStrLn $ "total trades processed: " ++ show totalTrades
      putStrLn $ "valid trades: " ++ show (totalTrades - length allErrors)
      putStrLn $ "invalid trades: " ++ show (length allErrors)
      putStrLn $ "final portfolio cash: $" ++ show (cash finalPortfolio)
      putStrLn $ "final positions: " ++ show (positions finalPortfolio)
      
      if null allErrors
        then putStrLn "\nall trades are valid!"
        else do
          putStrLn "\nvalidation errors found:"
          mapM_ (\err -> putStrLn $ "  " ++ err) allErrors

validateTradeSequence :: (Portfolio, [String]) -> Trade -> (Portfolio, [String])
validateTradeSequence (portfolio, errors) trade =
  let basicErrors = validate trade
      riskErrors = validateRisk trade portfolio defaultRiskConfig
      newErrors = errors ++ map (\err -> "trade " ++ show (tradeId trade) ++ ": " ++ err) (basicErrors ++ riskErrors)
      newPortfolio = if null (basicErrors ++ riskErrors) then updatePortfolio trade portfolio else portfolio
  in (newPortfolio, newErrors)
