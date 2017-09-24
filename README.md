# `configurator-applicative`

> The for-itself is the in-itself losing itself as in-itself in order to found itself as consciousness.

Self-describing parsers for configurator files. Never waste time wondering what a config should look like again!

## Motivation

`configurator` is a pretty nice library for parsing config files, but one problem that arises when working with it in a team is that the config file format is a by-product of the parsing code. If a teammate adds parsing for a new key, the only ways for you to know how to populate that key are to read through the parsing source or ask your teammates.

It doesn't have to be this way. For example, `optparse-applicative` is a great command line argument parsing library, but you don't need to read Haskell code to figure out the expectations of an implemented parser. The selling point of the library is that you get `--help` for free! `optparse-applicative` parsers are able to describe themselves by inspecting their structure. `configurator-applicative` aims to do this same sort of thing but with config file parsers. In fact, the `configurator-applicative`'s design is pretty shamelessly inspired by `optparse-applicative`!

**This is all still very WIP!**

### Example
The `examples` binary has an example parser:

```haskell
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
```

Asking `combinedParser` for the flat list of keys gives you this:
```
person.first :: Text -- their first name
person.last :: Text -- their last name
person.age :: Int -- their age
database.type :: Text -- rdbms | file | in-memory
database.user :: Text -- postgres user
database.password :: Text -- postgres password
database.path :: Text -- relative path to where files should be stored
```

Asking it for nested keys gives you this:
```
database
  type :: Text -- rdbms | file | in-memory
  user :: Text -- postgres user
  password :: Text -- postgres password
  path :: Text -- relative path to where files should be stored
person
  first :: Text -- their first name
  last :: Text -- their last name
  age :: Int -- their age
```
