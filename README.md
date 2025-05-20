# Silicon Gallery

A command line utility used to watch list of photo albums published
by [Silicon Hill](https://www.siliconhill.cz/photogalleries)
and send them to a Discord channel.

## Implementation milestones
- read configuration from environment variables
- fetch HTML
- parse HTML (using a library) and extract models
- File IO - check already published photo albums
- Discord API - send a message (HTTP endpoint call with custom flags)
- File IO - store that a new album was just posted
- repeat every n minutes
- build on CI

## Used Haskell language extensions

```haskell
-- Have to be added
{-# LANGUAGE DeriveDataTypeable #-} -- Allows automatic derivation of the `Data` and `Typeable` type classes.
{-# LANGUAGE DeriveGeneric #-}      -- Allows automatic derivation of the `Generic` type class.
{-# LANGUAGE GADTs #-}              -- Extends Haskell's type system to allow Generalized Algebraic Data Types.
{-# LANGUAGE OverloadedStrings #-}   -- Allows string literals to be interpreted as instances of `IsString`.
{-# LANGUAGE RankNTypes #-}         -- Allows quantification of types at arbitrary positions (rank-N types).

-- Enabled globally
{-# LANGUAGE DuplicateRecordFields #-} -- Allows record fields with the same name in different data types.
{-# LANGUAGE OverloadedRecordDot #-} -- Enables accessing record fields using the dot notation (e.g., `record.field`).
{-# LANGUAGE RecordWildCards #-}     -- Allows all fields of a record to be brought into scope without listing them individually.
{-# LANGUAGE NamedFieldPuns #-}      -- Allows pattern matching on specific record fields by name, even if others are present.
{-# LANGUAGE MultiParamTypeClasses #-} -- Enables type classes to have more than one type parameter.
```

## License

Silicon Gallery is licensed under the `GNU GPL v3.0` license.
