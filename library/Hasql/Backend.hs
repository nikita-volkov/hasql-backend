-- |
-- An open API for implementation of specific backend drivers.
module Hasql.Backend where

import Hasql.Backend.Prelude


-- * Connection
-------------------------

class Cx c where
  -- |
  -- Connection settings.
  type CxSettings c
  -- |
  -- A connection acquisition error.
  type CxError c
  acquireCx :: CxSettings c -> IO (Either (CxError c) c)
  releaseCx :: c -> IO ()


-- * Results
-------------------------

-- | A raw value returned from the database.
data family ResultValue c

-- | A raw result row.
type ResultRow c =
  Vector (ResultValue c)

-- |
-- A stream of rows of a result.
type ResultStream c =
  ListT IO (ResultRow c)

-- |
-- A matrix of a result.
type ResultMatrix c =
  Vector (ResultRow c)


-- * Statements
-------------------------

-- | 
-- A statement template with values for placeholders
-- and a flag, defining, whether it is preparable.
data Stmt c =
  Stmt {
    stmtTemplate :: !Text,
    stmtParams :: !(Vector (StmtParam c)),
    stmtPreparable :: !Bool
  }

-- | A prepared statement parameter.
data family StmtParam c


-- * Mapping
-------------------------

-- |
-- A support by a backend of mapping a specific data type.
class CxValue c v where
  encodeValue :: v -> StmtParam c
  decodeValue :: ResultValue c -> Either Text v


-- * Transaction
-------------------------

-- | A transaction execution support.
class CxTx c where
  -- | A transaction error.
  type TxError c
  -- | 
  -- Given an established connection execute a transaction,
  -- returning either an error or a maybe result,
  -- in which a 'Nothing' indicates that the transaction should be retried.
  runTx :: c -> TxMode -> Tx c a -> IO (Either (TxError c) (Maybe a))

-- |
-- For reference see
-- <https://en.wikipedia.org/wiki/Isolation_(database_systems)#Isolation_levels the Wikipedia info>.
data TxIsolationLevel =
  RepeatableReads |
  Serializable |
  ReadCommitted |
  ReadUncommitted

-- |
-- A mode, defining how a transaction should be executed.
-- 
-- * @Just (isolationLevel, write)@ indicates that a database transaction
-- should be established with a specified isolation level and a write mode.
-- 
-- * @Nothing@ indicates that there should be no database transaction established on
-- the backend and therefore it should be executed with no ACID guarantees,
-- but also without any induced overhead.
type TxMode =
  Maybe (TxIsolationLevel, TxWriteMode)

-- |
-- * @Nothing@ indicates a \"read\" mode.
-- 
-- * @Just True@ indicates a \"write\" mode.
-- 
-- * @Just False@ indicates a \"write\" mode without committing.
-- This is useful for testing, 
-- allowing you to modify your database, 
-- producing some result based on your changes,
-- and to let Hasql roll all the changes back on the exit from the transaction.
type TxWriteMode =
  Maybe Bool

type Tx c = 
  FreeT (TxF c) (MaybeT (EitherT (TxError c) IO))

data TxF c x =
  UnitTx (Stmt c) x |
  CountTx (Stmt c) (Word64 -> x) |
  MaybeTx (Stmt c) (Maybe (ResultRow c) -> x) |
  VectorTx (Stmt c) (Vector (ResultRow c) -> x) |
  StreamTx (Stmt c) (ListT (Tx c) (ResultRow c) -> x)
  deriving (Functor)


unitTx :: Stmt c -> Tx c ()
unitTx s = liftF (UnitTx s ())

countTx :: Stmt c -> Tx c Word64
countTx s = liftF (CountTx s id)

maybeTx :: Stmt c -> Tx c (Maybe (ResultRow c))
maybeTx s = liftF (MaybeTx s id)

vectorTx :: Stmt c -> Tx c (Vector (ResultRow c))
vectorTx s = liftF (VectorTx s id)

streamTx :: Stmt c -> Tx c (ListT (Tx c) (ResultRow c))
streamTx s = liftF (StreamTx s id)



