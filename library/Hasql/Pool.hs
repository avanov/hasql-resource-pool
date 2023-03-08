{-| This module represents an extension to 'hasql-pool', it allows for a mix of dynamic credentials
    and static settings for connections in a pool.
    Due to the fact that the module tries to extend 'hasql-pool' rather than rewrite it, the exposed API relies on
    combining existing 'hasql' types with new types, whereas a rewrite would just extend the original types.
    It is done so to simplify maintenance of the extended functionality and make it more compatible with any
    future development of 'hasql-pool'.
-}
module Hasql.Pool
(   Pool
,   Settings(..)
,   UsageError(..)
,   ConnectionGetter
,   Stats(..)
,   stats
,   getPoolUsageStat
,   acquire
,   acquireWith
,   release
,   use
,   useWithObserver
,   withResourceOnEither
)
where

import qualified Data.Pool as ResourcePool
import qualified Data.Pool.Internal as Unstable
import           System.Clock (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)

import           Hasql.Pool.Prelude
import qualified Hasql.Connection
import qualified Hasql.Session
import           Hasql.Pool.Observer (Observed(..), ObserverAction)


-- |
-- A pool of open DB connections.
newtype Pool =
    Pool (ResourcePool.Pool (Either Hasql.Connection.ConnectionError Hasql.Connection.Connection))



type PoolSize         = Int
type PoolStripes      = Int
type ResidenceTimeout = NominalDiffTime

-- |
-- Connection getter action that allows for obtaining Postgres connection settings
-- via external resources such as AWS tokens etc.
type ConnectionGetter = IO (Either Hasql.Connection.ConnectionError Hasql.Connection.Connection)

-- |
-- Settings of the connection pool. Consist of:
--
-- * Pool-size.
--
-- * Timeout.
-- An amount of time for which an unused resource is kept open.
-- The smallest acceptable value is 0.5 seconds.
--
-- * Connection settings.
--
type Settings =
  (PoolSize, ResidenceTimeout, Hasql.Connection.Settings)

-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire settings@(_size, _timeout, connectionSettings) =
    acquireWith (Hasql.Connection.acquire connectionSettings) settings


-- |
-- Similar to 'acquire', allows for finer configuration.
acquireWith :: ConnectionGetter
            -> Settings
            -> IO Pool
acquireWith connGetter (maxSize, timeout, connectionSettings) =
    fmap Pool $ createPool connGetter release timeout maxSize
    where
        release = either (const (pure ())) Hasql.Connection.release


createPool :: IO a
           -> (a -> IO ())
           -> NominalDiffTime
           -> Int
           -> IO (ResourcePool.Pool a)
createPool create free idleTime maxResources = ResourcePool.newPool cfg where
    -- defaultPoolConfig create free cacheTTL maxResources = PoolConfig
    cfg = ResourcePool.defaultPoolConfig create free (realToFrac idleTime) maxResources


-- |
-- Release the connection-pool by closing and removing all connections.
release :: Pool -> IO ()
release (Pool pool) =
    ResourcePool.destroyAllResources pool


-- |
-- A union over the connection establishment error and the session error.
data UsageError
    =   ConnectionError Hasql.Connection.ConnectionError
    |   SessionError    Hasql.Session.QueryError
    deriving (Show, Eq)

-- |
-- Use a connection from the pool to run a session and
-- return the connection to the pool, when finished.
use :: Pool -> Hasql.Session.Session a -> IO (Either UsageError a)
use = useWithObserver Nothing

-- |
-- Same as 'use' but allows for a custom observer action. You can use it for gathering latency metrics.
useWithObserver :: Maybe ObserverAction
                -> Pool
                -> Hasql.Session.Session a
                -> IO (Either UsageError a)
useWithObserver observer (Pool pool) session =
    fmap (either (Left . ConnectionError) (either (Left . SessionError) Right)) $
    withResourceOnEither pool $
    traverse runQuery
    where
        runQuery dbConn = maybe action (runWithObserver action) observer
            where
                action = Hasql.Session.run session dbConn

        runWithObserver action doObserve = do
            let measure = getTime Monotonic
            start  <- measure
            result <- action
            end    <- measure
            let nsRatio  = 1000000000
                observed = Observed {   latency = toRational (toNanoSecs (end `diffTimeSpec` start) % nsRatio)
                                    }
            doObserve observed >> pure result


withResourceOnEither :: ResourcePool.Pool resource
                     -> (resource -> IO (Either failure success))
                     -> IO (Either failure success)
withResourceOnEither pool act = mask_ $ do
    (resource, localPool) <- ResourcePool.takeResource pool
    failureOrSuccess      <- act resource `onException` ResourcePool.destroyResource pool localPool resource
    case failureOrSuccess of
        Right success -> do
            ResourcePool.putResource localPool resource
            pure $ Right success
        Left failure -> do
            ResourcePool.destroyResource pool localPool resource
            pure $ Left failure


data Stats = Stats
    {   currentUsage  :: !Int
        -- ^ Current number of items.
    ,   available     :: !Int
        -- ^ Total items available for consumption.
    } deriving Show


stats :: Pool -> IO Stats
stats (Pool pool) = currentlyAvailablePerStripe >>= collect where
    -- attributes extraction and counting
    collect xs = pure $ Stats inUse avail where
        inUse = maxResources - avail
        avail = sum xs

    currentlyAvailablePerStripe = traverse id peekAvailable
    peekAvailable               = (fmap stripeAvailability) <$> allStripes    -- array of IO Int
    stripeAvailability ms       = maybe quotaPerStripe Unstable.available ms  -- if the stripe ref is uninitialised, count the default availability
    allStripes                  = peekStripe <$> Unstable.localPools pool     -- array of IO Maybe
    peekStripe                  = tryReadMVar . Unstable.stripeVar

    -- data from the pool
    quotaPerStripe              = maxResources `quotCeil` numStripes
    numStripes                  = length $ Unstable.localPools pool  -- can be 'sizeofSmallArray' but requires 'primitive' as dependency
    maxResources                = Unstable.poolMaxResources . Unstable.poolConfig $ pool
    quotCeil x y                = let (z, r) = x `quotRem` y in if r == 0 then z else z + 1  -- copied from 'Data.Pool.Internal'


getPoolUsageStat :: Pool -> IO PoolSize
getPoolUsageStat pool = stats pool >>= (pure . currentUsage)
