{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Text.Megaparsec
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Control.Monad
import Data.Traversable
import Parser
import RSS
import Scraper

main :: IO ()
main = do
    txts <- fetchAllMonths
    messages <- fmap concat $ for txts $ \txt -> do
        case runParser (many messageP) "input" txt of
            Left errBundle -> do
                putStrLn $ errorBundlePretty errBundle
                fail "An error occurred"
            Right result -> return result
    let theFeed = atom messages
    TL.writeFile "atom.xml" theFeed
