-- |
-- An open API for implementation of specific backend drivers.
module Hasql.Backend where

import Hasql.Backend.Prelude


data Error =
  -- |
  -- Cannot connect to a server.
  CantConnect Text |
  -- |
  -- The connection got interrupted.
  ConnectionLost Text |
  -- |
  -- Some kind of error on backend.
  ErroneousResult Text |
  -- |
  -- An unexpected or an unparsable result.
  UnexpectedResult Text |
  -- |
  -- An unparsable statement template.
  UnparsableTemplate Text |
  -- |
  -- A transaction concurrency conflict, 
  -- which indicates that it should be retried.
  TransactionConflict |
  -- |
  -- An operation, 
  -- which requires a database transaction was executed without one.
  NotInTransaction
  deriving (Show, Typeable)

instance Exception Error


-- |
-- For reference see
-- <https://en.wikipedia.org/wiki/Isolation_(database_systems)#Isolation_levels the Wikipedia info>.
data IsolationLevel =
  Serializable |
  RepeatableReads |
  ReadCommitted |
  ReadUncommitted


-- |
-- An isolation level and a boolean, 
-- defining, whether the transaction will perform the "write" operations.
type TransactionMode =
  (IsolationLevel, Bool)


-- |
-- A stream of rows of a result.
type Stream b =
  ListT IO (Vector (Result b))

-- |
-- A matrix of a result.
type Matrix b =
  Vector (Vector (Result b))


-- |
-- A statement template with values for placeholders
-- and a flag, defining, whether it is preparable.
type Statement b =
  (ByteString, [StatementArgument b], Bool)


class Backend b where
  -- |
  -- An argument prepared for a statement.
  data StatementArgument b
  -- |
  -- A raw value returned from the database.
  data Result b
  -- |
  -- A backend-specific connection.
  data Connection b
  -- |
  -- Open a connection using the backend's settings.
  connect :: b -> IO (Connection b)
  -- |
  -- Close the connection.
  disconnect :: Connection b -> IO ()
  -- |
  -- Execute a statement.
  execute :: Statement b -> Connection b -> IO ()
  -- |
  -- Execute a statement
  -- and get a matrix of results.
  executeAndGetMatrix :: Statement b -> Connection b -> IO (Matrix b)
  -- |
  -- Execute a statement
  -- and stream the results using a cursor.
  -- This function will only be used from inside of transactions.
  executeAndStream :: Statement b -> Connection b -> IO (Stream b)
  -- |
  -- Execute a statement,
  -- returning the amount of affected rows.
  executeAndCountEffects :: Statement b -> Connection b -> IO Word64
  -- |
  -- Start a transaction in the specified mode.
  beginTransaction :: TransactionMode -> Connection b -> IO ()
  -- |
  -- Finish the transaction, 
  -- while releasing all the resources acquired with 'executeAndStream'.
  --  
  -- The boolean defines whether to commit the updates,
  -- otherwise it rolls back.
  finishTransaction :: Bool -> Connection b -> IO ()


-- |
-- Support by a backend of a specific data type.
class Mapping b v where
  renderValue :: v -> StatementArgument b
  parseResult :: Result b -> Either Text v



