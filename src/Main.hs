{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Csv
import Data.ByteString.Lazy (ByteString)
import Data.Vector (Vector, toList)
import GHC.Generics (Generic)

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Data
import Debug.Trace (trace)

input :: ByteString
input = "fundName,cmRet\nUS Stock Market Index,2.83\nSmall Cap Growth ETF,1.83\n"

main :: IO ()
main = do
     let decoded = decodeByName input :: Either String (Header, Vector Row)
         rows = toList $ case decoded of
                            Left err -> error err
                            Right (_, rows) -> rows
     rendered <- mapM renderRow rows
     mapM_ print rendered

renderRow row = render (mkGenericContext row)

data StockChange = Positive | Negative
type FundName = String
type Percentage = Float

data Row = Row {
      fundName :: !FundName
    , cmRet    :: !Percentage
--    , rowChange   :: !StockChange
} deriving (Show, Generic, Data, Typeable)

instance FromNamedRecord Row
instance DefaultOrdered Row


render = hastacheStr defaultConfig (encodeStr template)

data Info = Info {
    name    :: String,
    unread  :: Int
    } deriving (Data, Typeable)

template = "Hello, {{fundName}}, your cm ret is {{cmRet}}!"