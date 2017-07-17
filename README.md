Self-describing parsers for configurator files.

Never waste time wondering what a config should look like again!

### Example
The `examples` binary has an example parser. Asking for the flat list of keys gives you this:
```
person.first :: Text -- their first name
person.last :: Text -- their last name
person.age :: Int -- their age
database.type :: Text -- rdbms | file | in-memory
database.user :: Text -- postgres user
database.password :: Text -- postgres password
database.path :: Text -- relative path to where files should be stored
```

Asking for it nested gives you this:
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