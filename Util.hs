{-
    Zachary Weaver
    Util.hs
    Version 0.1.1
    
    Provides simple utilities
-}

module Util
    ( (??)
    , (.:)
    , getStateL
    , setStateL
    , modStateL
    , lift2
    ) where


import Data.Lens.Common
import Control.Monad.Trans.Class
import Control.Monad.State.Class

(??) :: a -> a -> Bool -> a
(a ?? b) tf = if tf then a else b

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

getStateL :: MonadState s m => Lens s a -> m a
getStateL theLens = get >>= return .  getL theLens

setStateL :: MonadState s m => Lens s a -> a -> m ()
setStateL = modify .: setL

modStateL :: MonadState s m => Lens s a -> (a -> a) -> m ()
modStateL = modify .: modL

lift2 m = lift $ lift m
