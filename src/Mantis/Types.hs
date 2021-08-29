-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Mantis types.
--
-----------------------------------------------------------------------------


{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Mantis.Types (
-- * Mantis monad
  MantisM(..)
, mantisM
, runMantisToIO
-- * Printing
, logMantis
, printMantis
, debugMantis
-- * Lifting and hoisting
, foistMantis
, foistMantisEither
, foistMantisEitherIO
, foistMantisExcept
, foistMantisExceptIO
, foistMantisMaybe
, foistMantisMaybeIO
-- * Exceptions
, throwMantis
-- * Slots
, SlotRef(..)
) where


import Control.Arrow (first)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Control.Monad.Trans.Except.Extra (hoistExceptT)
import System.IO (hPutStrLn, stderr)


-- | Mantis monad.
newtype MantisM m a = MantisM {runMantisM :: ExceptT String m a}
  deriving (Applicative, Functor, Monad, MonadError String, MonadFail, MonadIO)


-- | Lift in the Mantis monad.
mantisM :: Monad m
        => MonadTrans t
        => MantisM m a
        -> t (MantisM m) a
mantisM = lift


-- | Run the Mantis monad to IO.
runMantisToIO :: MantisM IO a
              -> IO (Either String a)
runMantisToIO = runExceptT . runMantisM


-- | Pull a value into the Mantis monad.
foistMantis :: Monad m
            => a           -- ^ The value.
            -> MantisM m a -- ^ Action for the value.
foistMantis = MantisM . ExceptT . return . Right


-- | Pull a value into the Mantis monad.
foistMantisEither :: Monad m
                  => Show e
                  => Either e a  -- ^ The value.
                  -> MantisM m a -- ^ Action for the value.
foistMantisEither = MantisM . withExceptT show . ExceptT . return


-- | Pull a value into the Mantis monad.
foistMantisEitherIO :: MonadIO m
                    => Show e
                    => IO (Either e a) -- ^ The value.
                    -> MantisM m a     -- ^ Action for the value.
foistMantisEitherIO = MantisM . hoistExceptT liftIO . withExceptT show . ExceptT


-- | Pull a value into the Mantis monad.
foistMantisExcept :: Monad m
                  => Show e
                  => ExceptT e m a -- ^ The value.
                  -> MantisM m a   -- ^ Action for the value.
foistMantisExcept = MantisM . withExceptT show


-- | Pull a value into the Mantis monad.
foistMantisExceptIO :: MonadIO m
                    => Show e
                    => ExceptT e IO a -- ^ The value.
                    -> MantisM m a    -- ^ Action for the value.
foistMantisExceptIO = MantisM . hoistExceptT liftIO . withExceptT show


-- | Pull a value into the Mantis monad.
foistMantisMaybe :: Monad m
                 => String      -- ^ The error message.
                 -> Maybe a     -- ^ The value.
                 -> MantisM m a -- ^ Action for the value.
foistMantisMaybe message = MantisM . ExceptT . return . maybe (Left message) Right


-- | Pull a value into the Mantis monad.
foistMantisMaybeIO :: MonadIO m
                   => String       -- ^ The error message.
                   -> IO (Maybe a) -- ^ The value.
                   -> MantisM m a  -- ^ Action for the value.
foistMantisMaybeIO message = MantisM . hoistExceptT liftIO . ExceptT . fmap (maybe (Left message) Right)


-- | Throw an error.
throwMantis :: Monad m
            => String      -- ^ The error message.
            -> MantisM m a -- ^ Action for throwing the error.
throwMantis = MantisM . throwE


-- | Log a message.
logMantis :: MonadIO m
          => Bool         -- ^ Whether to log to standard output.
          -> String       -- ^ The message.
          -> MantisM m () -- ^ Action to log the message.
logMantis out =
  liftIO
    . if out
        then putStrLn
        else hPutStrLn stderr


-- | Print a message to standard output.
printMantis :: MonadIO m
            => String       -- ^ The message.
            -> MantisM m () -- ^ Action to print the message.
printMantis = logMantis True


-- | Print a message to standard error.
debugMantis :: MonadIO m
            => String       -- ^ The message.
            -> MantisM m () -- ^ Action to print the message.
debugMantis = logMantis False


-- | A reference to a slot.
data SlotRef =
    AbsoluteSlot Integer -- ^ The absolute slot number.
  | RelativeSlot Integer -- ^ The number of slots relative to the current tip.
    deriving (Eq, Ord, Show)

instance Read SlotRef where
  readsPrec p ('+' : remainder) = first RelativeSlot <$> readsPrec p remainder
  readsPrec p        remainder  = first AbsoluteSlot <$> readsPrec p remainder
