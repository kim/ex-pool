{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module:      Data.Pool
-- Copyright:   (c) 2013 Kim Altintop, (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Kim Altintop <kim.altintop@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- A high-performance striped pooling abstraction for managing flexibly-sized
-- collections of resources such as database connections.
--
-- This module is based on @resource-pool@. For more comprehensive
-- documentation, please refer to the original package:
-- <http://hackage.haskell.org/package/resource-pool>
--
module Data.Pool
    ( Pool(nStripes, idleTime, maxResources)
    , LocalPool
    , createPool
    , destroyResource
    , purgePool
    , putResource
    , takeResource
    , tryTakeResource
    , tryWithResource
    , withResource
    )
where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch    (MonadCatch)
import qualified Control.Monad.Catch    as E
import           Control.Monad.IO.Class
import           Data.Hashable          (hash)
import           Data.IORef             (IORef, mkWeakIORef, newIORef)
import           Data.List              (partition)
import           Data.Time.Clock
import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as V
import           Data.Word


data Resource a = Resource
    { resource :: !a
    , lastUse  :: !UTCTime
    }

data LocalPool a = LocalPool
    { inUse     :: !(TVar Word32)
    , resources :: !(TVar [Resource a])
    , lfin      :: !(IORef ())
    }

data Pool a = Pool
    { create       :: IO a
    , destroy      :: a -> IO ()
    , nStripes     :: !Word32
    , idleTime     :: !NominalDiffTime
    , maxResources :: !Word32
    , localPools   :: !(Vector (LocalPool a))
    , fin          :: !(IORef ())
    }

instance Show (Pool a) where
    show p = "Pool {nStripes = " ++ show (nStripes p) ++ ", " ++
             "idleTime = " ++ show (idleTime p) ++ ", " ++
             "maxResources = " ++ show (maxResources p) ++ "}"

createPool :: IO a
           -- ^ Action to create a new resource
           -> (a -> IO ())
           -- ^ Action to destroy a resource
           -> Word32
           -- ^ Stripe count
           -> NominalDiffTime
           -- ^ Amount of time after which an unused resource can be released
           -> Word32
           -- ^ Maximum number of resources per stripe
           -> IO (Pool a)
createPool create destroy nStripes idleTime maxResources = do
    locals <- V.replicateM (fromIntegral nStripes) $  LocalPool
                                                  <$> newTVarIO 0
                                                  <*> newTVarIO []
                                                  <*> newIORef  ()
    reapId <- forkIO $ reaper destroy idleTime locals
    pool   <- Pool create destroy nStripes idleTime maxResources locals
              <$> newIORef ()

    addFinalizer (fin pool) $ do
        killThread reapId
        purgePool pool
    V.forM_ locals $ \ lp -> addFinalizer (lfin lp) $ purgeLocalPool destroy lp

    return pool
  where
    addFinalizer ref = void . mkWeakIORef ref

-- | Destroys all resources currently not in use and removes them from the pool.
--
-- Note that resources are automatically released when the 'Pool' is
-- garbage-collected. This function is however useful in situations where a
-- 'Pool' is explicitly discarded and resources should be freed immediately.
purgePool :: Pool a -> IO ()
purgePool p = V.forM_ (localPools p) $ purgeLocalPool (destroy p)

withResource :: (MonadIO m, MonadCatch m) => Pool a -> (a -> m b) -> m b
{-# SPECIALIZE withResource :: Pool a -> (a -> IO b) -> IO b #-}
withResource p act = E.mask $ \ restore -> do
    (r, lp) <- takeResource p
    res <- restore (act r) `E.onException` destroyResource p lp r
    putResource lp r
    return res
{-# INLINABLE withResource #-}

-- | Similar to 'withResource', but only performs the action if a resource could
-- be taken from the pool /without blocking/. Otherwise, 'tryWithResource'
-- returns immediately with 'Nothing' (ie. the action function is /not/ called).
-- Conversely, if a resource can be borrowed from the pool without blocking, the
-- action is performed and it's result is returned, wrapped in a 'Just'.
tryWithResource :: (MonadIO m, MonadCatch m)
                => Pool a -> (a -> m b) -> m (Maybe b)
{-# SPECIALIZE tryWithResource :: Pool a -> (a -> IO b) -> IO (Maybe b) #-}
tryWithResource p act = E.mask $ \ restore -> do
    mres <- tryTakeResource p
    case mres of
        Just (r, lp) -> do
            res <- restore (act r) `E.onException` destroyResource p lp r
            putResource lp r
            return (Just res)
        Nothing -> restore $ return Nothing
{-# INLINABLE tryWithResource #-}

takeResource :: MonadIO m => Pool a -> m (a, LocalPool a)
{-# SPECIALIZE takeResource :: Pool a -> IO (a, LocalPool a) #-}
takeResource p = do
    lp <- getLocalPool p
    r  <- liftIO . join . atomically $ do
        rs <- readTVar (resources lp)
        case rs of
            (x:xs) -> do
                writeTVar (resources lp) xs
                return . return . resource $ x
            [] -> do
                used <- readTVar (inUse lp)
                when (used == maxResources p) retry
                writeTVar (inUse lp) $! used + 1
                return $ liftIO (create p)
                    `E.onException` modify_ (inUse lp) (subtract 1)
    return (r, lp)
{-# INLINABLE takeResource #-}

-- | A non-blocking version of 'takeResource'. The 'tryTakeResource' function
-- returns immediately, with 'Nothing' if the pool is exhausted, or @'Just' (a,
-- 'LocalPool' a)@ if a resource could be borrowed from the pool successfully.
tryTakeResource :: MonadIO m => Pool a -> m (Maybe (a, LocalPool a))
{-# SPECIALIZE tryTakeResource :: Pool a -> IO (Maybe (a, LocalPool a)) #-}
tryTakeResource p = do
    lp <- getLocalPool p
    r  <- liftIO . join . atomically $ do
        rs <- readTVar (resources lp)
        case rs of
            (x:xs) -> do
                writeTVar (resources lp) xs
                return . return . Just . resource $ x
            [] -> do
                used <- readTVar (inUse lp)
                if used == maxResources p
                  then return . return $ Nothing
                  else do
                      writeTVar (inUse lp) $! used + 1
                      return $ Just
                            <$> liftIO (create p)
                                `E.onException` modify_ (inUse lp) (subtract 1)
    return $ flip (,) lp <$> r
{-# INLINABLE tryTakeResource #-}

putResource :: MonadIO m => LocalPool a -> a -> m ()
{-# SPECIALIZE putResource :: LocalPool a -> a -> IO () #-}
putResource lp r = liftIO $ do
    now <- getCurrentTime
    atomically $ modifyTVar' (resources lp) (Resource r now:)
{-# INLINABLE putResource #-}

destroyResource :: MonadIO m => Pool a -> LocalPool a -> a -> m ()
{-# SPECIALIZE destroyResource :: Pool a -> LocalPool a -> a -> IO () #-}
destroyResource p lp r = liftIO $ do
    ignoreExceptions $ destroy p r
    modify_ (inUse lp) (subtract 1)
{-# INLINABLE destroyResource #-}

--------------------------------------------------------------------------------
--                              Internal                                      --
--------------------------------------------------------------------------------

reaper :: (a -> IO ()) -> NominalDiffTime -> Vector (LocalPool a) -> IO ()
reaper destroy idleTime pools = forever $ do
    threadDelay (1 * 1000000)
    now <- getCurrentTime
    let isStale r = now `diffUTCTime` lastUse r > idleTime
    V.forM_ pools $ \ (LocalPool inUse resources _) -> do
        rs <- atomically $ do
            (stale,fresh) <- partition isStale <$> readTVar resources
            unless (null stale) $ do
                writeTVar resources fresh
                modifyTVar' inUse $ subtract (fromIntegral (length stale))
            return (map resource stale)
        forM_ rs $ liftIO . ignoreExceptions . destroy

purgeLocalPool :: (a -> IO ()) -> LocalPool a -> IO ()
purgeLocalPool destroy (LocalPool inUse resources _) = do
    rs <- atomically $ do
        rs <- readTVar resources
        modifyTVar' inUse     $ subtract (fromIntegral (length rs))
        modifyTVar' resources $ const []
        return rs
    forM_ rs $ liftIO . ignoreExceptions . destroy . resource
{-# INLINABLE purgeLocalPool #-}

getLocalPool :: MonadIO m => Pool a -> m (LocalPool a)
{-# SPECIALIZE getLocalPool :: Pool a -> IO (LocalPool a) #-}
getLocalPool p = do
    i <- liftIO $ ((`mod` fromIntegral (nStripes p)) . hash) <$> myThreadId
    return $ localPools p ! i
{-# INLINABLE getLocalPool #-}

modify_ :: TVar a -> (a -> a) -> IO ()
modify_ t f = atomically $ modifyTVar' t f
{-# INLINABLE modify_ #-}

ignoreExceptions :: IO () -> IO ()
ignoreExceptions = E.handleAll (const $ return ())
{-# INLINABLE ignoreExceptions #-}
