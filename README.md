Self-describing parsers for configurator files.

Never waste time wondering what a config should look like again!

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

**This is all still very WIP!**