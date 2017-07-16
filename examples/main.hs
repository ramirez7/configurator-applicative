{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Data.Text                     (Text)
import           Data.Typeable
import           System.Environment            (getArgs)

import           Data.Configurator.Applicative

main :: IO ()
main = getArgs >>= \case
  ["flat"] -> printKeysFlat combinedParser
  ["nested"] -> printKeysNested combinedParser
  ["parse", path] -> putStrLn "unimplemented" >> pure ()
  _ -> putStrLn "unrecognized args"

data Person = Person
  { firstName :: Text
  , lastName  :: Text
  , age       :: Int
  } deriving (Eq, Show, Typeable)

personParser :: Parser Person
personParser =
  Person <$> require "person.first" "their first name"
         <*> require "person.last" "their last name"
         <*> require "person.age" "their age"

data DatabaseConfig =
    RDBMS { user :: Text, password :: Text }
  | File { path :: Text }
  | InMemory deriving (Eq, Show, Typeable)

databaseConfigParser :: Parser DatabaseConfig
databaseConfigParser = fork (require "database.type" "rdbms | file | in-memory") $
  [ ("rdbms" :: Text, RDBMS <$> require "database.user" "postgres user" <*> require "database.password" "postgres password")
  , ("file", File <$> require "database.path" "relative path to where files should be stored")
  , ("in-memory", pure InMemory)
  ]

combinedParser :: Parser (Person, DatabaseConfig)
combinedParser = (,) <$> personParser <*> databaseConfigParser
