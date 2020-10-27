{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Scraper where

import qualified Data.Text.Lazy as TL
import Control.Lens
import Data.Text.Lazy.Lens
import Network.Wreq
import Control.Monad
import Control.Applicative
import Control.Exception

-- scrapeAll :: IO ()
-- scrapeAll = do

months :: [String]
months =
  [ "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  ]

mailingListLinks :: [String]
mailingListLinks = do
        year :: Int <- [2020..]
        month <- months
        return . concat $ ["https://mail.haskell.org/pipermail/haskell-cafe/", show year, "-", month, ".txt"]

type URL = String

fetchMessages :: URL -> IO TL.Text
fetchMessages url = do
    putStrLn $ "Fetching " <> url
    response <- catch @SomeException (get url) (const $ fail "Doesn't exist")
    let code = response ^. responseStatus . statusCode
    when (code == 404) $ fail "Doesn't exist"
    return (response ^. responseBody . utf8)

tryAll :: (Monad f, Alternative f) => [f a] -> f [a]
tryAll [] = pure []
tryAll (x : xs) =
    optional x >>= \case
      Nothing -> return []
      Just a -> (a:) <$> tryAll xs

fetchAllMonths :: IO [TL.Text]
fetchAllMonths = do
    tryAll (fetchMessages <$> mailingListLinks)
