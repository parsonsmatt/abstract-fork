{-# language MagicHash #-}
{-# language StrictData #-}
{-# language RankNTypes #-}
{-# language DefaultSignatures #-}
{-# language UnboxedTuples #-}

-- | An abstraction for forking threads.
--
-- Prefer to use 'MonadFork' and 'fork' instead of @'liftIO' . 'forkIO'@,
-- so that you can control the forking behavior of your application.
module Control.Monad.Fork
    (  -- * The Type Class
      MonadFork(..)
    , -- * Convenience Functions
      fork
    , forkIO
    , forkOn
    , forkWithUnmask
    , rawFork
      -- * Customizing Fork Behavior
    , ForkOptions
    , forkOnCapability
    , forkExceptionHandler
    ) where

import qualified Control.Concurrent as CC
import GHC.Int (Int(..))
import GHC.IO (IO(..), unsafePerformIO, unsafeUnmask)
import qualified GHC.Conc.Sync as CC
import Control.Concurrent (ThreadId)
import Control.Monad.Catch
import Control.Exception (AsyncException(..), BlockedIndefinitelyOnMVar(..), BlockedIndefinitelyOnSTM(..))
import Control.Monad.IO.Class
import GHC.Exts (fork#, forkOn#)
import Control.Monad.IO.Unlift

-- | The options for forking a thread.
data ForkOptions m = ForkOptions
    { forkOnCapability :: Maybe Int
    -- ^ The capability to fork on. If 'Nothing' is given, then it runs on
    -- an unbound green thread. If @'Just' cpu@ is provided, then it runs
    -- on the CPU thread and is bound. See the documentation on 'CC.forkOn'
    -- for more details.
    , forkExceptionHandler :: Maybe (SomeException -> m ())
    -- ^ The exception handler to install for the forked thread. Given
    -- 'Nothing', this will avoid installing an exception handler. This is
    -- useful if you have high performance needs.
    }

-- | The default options for forking a thread. Installs the same exception
-- handler that 'forkIO' uses.
defaultForkOptions :: MonadCatch m => ForkOptions m
defaultForkOptions =
    rawForkOptions
        { forkExceptionHandler =
            Just childHandler
        }

rawForkOptions :: ForkOptions m
rawForkOptions =
    ForkOptions
        { forkOnCapability =
            Nothing
        , forkExceptionHandler =
            Nothing
        }

-- | This class defines a primitive method for forking threads. The
-- function here allows you to specify and customize how forking happens in
-- your application, or allows you to generalize the forking mechanism in
-- your library.
--
-- There are two main motivations:
--
-- @ResourceT@. The default implementation of 'forkIO' causes problems.
-- Therefore, there is a 'resourceForkIO' function which can prevent that.
-- Unfortunately, it's very difficult to remember and communicate this,
-- since @liftIO . forkIO@ is easy and obvious to write.
--
-- @hs-opentelemetry@. This library uses a hack to store a thread-local
-- context and uses that to provide seamlessly instrumented versions of
-- common libraries. Unfortunately, that context is lost on regular
-- 'forkIO'. You can get around that by defining a custom fork:
--
-- @
-- forkIOWithContext :: IO () -> IO ThreadId
-- forkIOWithContext action = do
--   context <- getCurrentContext
--   forkIO $ do
--      _  <- attachContext context
--      action
-- @
--
-- However, this @forkIOWithContext@ can easily by bypassed. A call to plain @async@
-- or @mapConcurrently@ will lose the context, since they don't use this
-- fork function to begin with.
--
-- By generalizing functions like @async@ on this class, you can allow your
-- code to use the custom fork function - like @resourceForkIO@ or
-- @forkIOWithContext@.
--
-- The default implementation is based on 'MonadUnliftIO'. This uses the
-- default 'CC.forkIO' behavior.
class Monad m => MonadFork m where
    -- | Fork a thread with the given 'ForkOptions' and provide it an
    -- @unmask@ parameter. The @unmask@ parameter will allow asynchronous
    -- exceptions to be delivered to the thread, even if you're in an
    -- 'uninterruptibleMask'.
    --
    -- Example call would look like this:
    --
    -- @
    -- forkWithOptionsWithUnmask 'defaultForkOptions' $ \\ unmask -> do
    --     putStrLn "I'm in a forked thread!"
    --     unmask $ do
    --         putStrLn "I might be in danger..."
    -- @
    forkWithOptionsWithUnmask
        :: ForkOptions m
        -> ((forall a. m a -> m a) -> m () )
        -- ^ An action accepting an @unmask@ parameter.
        -> m ThreadId
    default forkWithOptionsWithUnmask
        :: (MonadUnliftIO m, MonadCatch m)
        => ForkOptions m
        -> ((forall a. m a -> m a) -> m () )
        -> m ThreadId
    forkWithOptionsWithUnmask opts action =
        withRunInIO $ \runInIO -> do
            let rawForkFn =
                    case forkOnCapability opts of
                        Nothing ->
                            rawForkIO
                        Just i ->
                            rawForkOnIO i
            let exnHandler =
                    case forkExceptionHandler opts of
                        Nothing ->
                            id
                        Just handler ->
                            handle handler
            rawForkFn $
                runInIO $
                    exnHandler $
                        action
                            (\x -> liftIO (unsafeUnmask (runInIO x)))

instance MonadFork IO

-- | Fork a thread using the given 'ForkOptions'.
forkWithOptions :: MonadFork m => ForkOptions m -> m () -> m ThreadId
forkWithOptions opts action =
    forkWithOptionsWithUnmask opts (\_ -> action)

-- Implementation copied from the @async@ package internals.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO (IO action) = IO $ \ s ->
   case (fork# action s) of
       (# s1, tid #) ->
           (# s1, CC.ThreadId tid #)

{-# INLINE rawForkOnIO #-}
rawForkOnIO :: Int -> IO () -> IO ThreadId
rawForkOnIO (I# i) (IO action) = IO $ \ s ->
   case (forkOn# i action s) of
       (# s1, tid #) ->
           (# s1, CC.ThreadId tid #)

-- | An alias for 'fork'. Used to help aid migrations.
forkIO :: (MonadCatch m, MonadFork m) => m () -> m ThreadId
forkIO = fork

-- | Fork a thread with the 'defaultForkOptions'. This uses the same
-- exception handler that is provided with 'CC.forkIO', so you should get
-- consistent behavior with that.
fork :: (MonadCatch m, MonadFork m) => m () -> m ThreadId
fork = forkWithOptions defaultForkOptions

-- | Fork a thread with the 'rawForkOptions'. This does not install an
-- exception handler, which may result in a performance improvement.
rawFork :: (MonadFork m) => m () -> m ThreadId
rawFork = forkWithOptions rawForkOptions

-- | Fork a thread with the 'defaultForkOptions' and provide an @unmask@
-- function.
--
-- @
-- 'mask_' $ do
--     -- we are now masked, so can safely install an exception handler
--     forkWithUnmask $ \\ unmask -> do
--         eresult <- 'try' $ unmask $ do
--             -- we are unmasked, so async exceptions can happen
--             -- but they will be caught with try
--             doSomething
--         -- we are back to a masked state
--         handler eresult
-- @
--
-- See 'CC.forkWithUnmask' for more details.
forkWithUnmask
    :: (MonadCatch m, MonadFork m)
    => ((forall a. m a -> m a) -> m ()) -> m ThreadId
forkWithUnmask =
    forkWithOptionsWithUnmask defaultForkOptions

-- | Fork a thread with the 'defaultForkOptions' on the capability
-- provided.
--
-- See 'CC.forkOn' for more details.
forkOn :: (MonadCatch m, MonadFork m) => Int -> m () -> m ThreadId
forkOn i = forkWithOptions defaultForkOptions { forkOnCapability = Just i }

-- Copied from base
childHandler :: MonadCatch m => SomeException -> m ()
childHandler err = catch (real_handler err) childHandler
  -- We must use catch here rather than catchException. If the
  -- raised exception throws an (imprecise) exception, then real_handler err
  -- will do so as well. If we use catchException here, then we could miss
  -- that exception.

-- copied from base
real_handler :: MonadThrow m => SomeException -> m ()
real_handler se
  | Just BlockedIndefinitelyOnMVar <- fromException se  =  return ()
  | Just BlockedIndefinitelyOnSTM  <- fromException se  =  return ()
  | Just ThreadKilled              <- fromException se  =  return ()
  | Just StackOverflow             <- fromException se  =  return $ unsafePerformIO CC.reportStackOverflow
  | otherwise                                           =  return $ unsafePerformIO $ CC.reportError se
