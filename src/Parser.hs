{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Parser where


import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Control.Monad
-- import Control.Applicative.Combinators

type Parser a = Parsec Void TL.Text a

data Header = Header
    {   author :: TL.Text
      , subject :: TL.Text
      , messageID :: TL.Text
      , inReplyTo :: Maybe TL.Text
      , references :: Maybe TL.Text
      , date :: TL.Text
    }

data Message =
    Message
      { content :: TL.Text
      , author :: TL.Text
      , subject :: TL.Text
      , messageID :: TL.Text
      , inReplyTo :: Maybe TL.Text
      , references :: Maybe TL.Text
      , date :: TL.Text
      } deriving (Show)


preambleP :: Parser ()
preambleP = do
    void $ string "From"
    void $ skipManyTill (anySingleBut '\n') newline

authorP :: Parser TL.Text
authorP = do
    void $ string "From: "
    void $ skipMany (anySingleBut '(')
    name <- TL.pack <$> between (char '(') (char ')') (many (anySingleBut ')'))
    newline
    return name

lineRemainderP :: Parser TL.Text
lineRemainderP = do
    prefix <- singleLineRemainder
    rest <- many (hspace1 *> singleLineRemainder)
    return $ TL.intercalate " " (prefix : rest)
  where
    singleLineRemainder =
        TL.pack <$> someTill anySingle newline

dateP :: Parser TL.Text
dateP = do
    void $ string "Date: "
    lineRemainderP

subjectP :: Parser TL.Text
subjectP = do
    void $ string "Subject: "
    lineRemainderP

inReplyToP :: Parser TL.Text
inReplyToP = do
    void $ string "In-Reply-To: "
    lineRemainderP

referencesP :: Parser TL.Text
referencesP = do
    void $ string "References: "
    lineRemainderP

messageIDP :: Parser TL.Text
messageIDP = do
    void $ string "Message-ID: "
    lineRemainderP

contentP :: Parser TL.Text
contentP = do
    TL.pack <$> someTill anySingle (lookAhead (void $ try headerP) <|> eof)

headerP :: Parser Header
headerP = do
    preambleP
    author <- authorP
    date <- dateP
    subject <- subjectP
    inReplyTo <- optional inReplyToP
    references <- optional referencesP
    messageID <- messageIDP
    return $ Header {..}

messageP :: Parser Message
messageP = do
    Header {..} <- headerP
    content <- contentP
    return $ Message {..}

testParser :: IO ()
testParser = do
    txt <- TL.readFile "./data/may.txt"
    case runParser (many messageP) "input" txt of
        Left errBundle -> putStrLn $ errorBundlePretty errBundle
        Right result -> print $ result
