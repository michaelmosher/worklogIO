{-# LANGUAGE OverloadedStrings #-}

module Jira.Data ( JiraConfig(..),
                   Worklog(..),
                   worklogs ) where

import Data.Aeson (FromJSON, Value, (.:), parseJSON, withObject)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.Text as T (Text, splitOn, tail)
import qualified Data.Text.Read as TR (decimal)
import Data.Time (Day, fromGregorianValid)

data JiraConfig = JiraConfig {
    hostname :: T.Text,
    username :: ByteString,
    password :: ByteString
} deriving (Show)

worklogs :: Value -> Parser [Worklog]
worklogs = withObject "worklogs" (.: "worklogs")

newtype Seconds = Seconds Integer deriving (Show)

data Worklog = Worklog {
    authorName :: T.Text,
    comment    :: T.Text,
    timeLogged :: Seconds,
    dateLogged :: Day
} deriving (Show)

instance FromJSON Worklog where
    parseJSON = withObject "worklog" $ \w -> do
        author <- w .: "author"
        authorName' <- author .: "name"
        comment' <- w .: "comment"
        timeLogged' <- w .: "timeSpentSeconds"

        started <- w .: "started"
        let date = readISODateText started
        case date of
            Nothing -> fail "date parse failed"
            Just d -> return (Worklog authorName' comment' (Seconds timeLogged') d)

readISODateText :: T.Text -> Maybe Day
readISODateText datetime =
    let date = head $ T.splitOn "T" datetime
        (year, monthAndDay) = unpackTextReader $ TR.decimal date
        (month, andDay) = unpackTextReader $ TR.decimal (T.tail monthAndDay) -- strip leading '-'
        (day, _) = unpackTextReader $ TR.decimal (T.tail andDay) -- strip leading '-'
    in fromGregorianValid year month day

unpackTextReader :: Integral a => Either String (a, T.Text) -> (a, T.Text)
unpackTextReader (Left _) = (0, "")
unpackTextReader (Right t) = t