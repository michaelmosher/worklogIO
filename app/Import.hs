{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ini.Config (parseIniFile)
import qualified Data.Text.IO as T (readFile)
import System.Exit (exitFailure)

import qualified Jira

main :: IO ()
main = do
    jCfg <- T.readFile "./app/worklogIO.ini"
    case parseIniFile jCfg Jira.configParser of
        Left s -> do
            putStrLn s
            exitFailure
        Right j -> Jira.getWorklogs j 27120
            >>= mapM_ print . Jira.unpackWorklogs
