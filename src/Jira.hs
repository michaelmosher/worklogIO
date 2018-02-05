{-# LANGUAGE OverloadedStrings #-}

module Jira ( getWorklogs,
              configParser,
              unpackWorklogs ) where

import Control.Exception (throwIO)
import Data.Aeson (decode)
import Data.Aeson.Types (parseMaybe)
import Data.Ini.Config (IniParser, fieldOf, section, string)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text, pack)
import qualified Network.HTTP.Req as HTTP
import Network.HTTP.Req ((/:))

import Jira.Data (JiraConfig(..), Worklog, worklogs)

instance HTTP.MonadHttp IO where
    handleHttpException = throwIO

configParser :: IniParser JiraConfig
configParser =
    section "Jira" $ do
        host <- fieldOf "hostname" string
        user <- fieldOf "username" string
        pass <- fieldOf "password" string
        return (JiraConfig host user pass)

getWorklogs :: (HTTP.MonadHttp m)
            => JiraConfig
            -> Int
            -> m HTTP.LbsResponse
getWorklogs cfg issueId =
    let issue = T.pack (show issueId) -- Int to String to Text
    in jiraGetRequest cfg ["rest", "api", "2", "issue", issue, "worklog"]

unpackWorklogs :: HTTP.LbsResponse -> [Worklog]
unpackWorklogs resp =
    fromMaybe [] $ parseMaybe worklogs =<< decode (HTTP.responseBody resp)

jiraGetRequest :: (HTTP.MonadHttp m)
               => JiraConfig
               -> [T.Text]
               -> m HTTP.LbsResponse
jiraGetRequest cfg urlParts =
    let host = HTTP.https (hostname cfg)
        url = foldl (/:) host urlParts
        creds = HTTP.basicAuth (username cfg) (password cfg)
    in HTTP.req HTTP.GET url HTTP.NoReqBody HTTP.lbsResponse creds
