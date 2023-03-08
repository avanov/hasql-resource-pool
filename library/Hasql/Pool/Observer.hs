module Hasql.Pool.Observer
(   Observed(..)
,   ObserverAction
)
where

import Hasql.Pool.Prelude


-- | Represents properties of an observed IO action associated with a pool item
newtype Observed = Observed
    {   latency :: Rational
    } deriving (Show)


type ObserverAction = Observed -> IO ()
