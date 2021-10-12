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
-- | Mantra types.
--
-----------------------------------------------------------------------------


{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Mantra.Types (
-- * Mantra monad
  MantraM(..)
, mantraM
, runMantraToIO
-- * Printing
, logMantra
, printMantra
, debugMantra
-- * Lifting and hoisting
, foistMantra
, foistMantraEither
, foistMantraEitherIO
, foistMantraExcept
, foistMantraExceptIO
, foistMantraMaybe
, foistMantraMaybeIO
-- * Exceptions
, throwMantra
-- * Slots
, SlotRef(..)
) where


import Control.Arrow                    (first)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Except             (MonadError)
import Control.Monad.Trans              (MonadTrans, lift)
import Control.Monad.Trans.Except       (ExceptT(..), runExceptT, throwE, withExceptT)
import Control.Monad.Trans.Except.Extra (hoistExceptT)
import System.IO                        (hPutStrLn, stderr)


-- | Mantra monad.
newtype MantraM m a = MantraM {runMantraM :: ExceptT String m a}
  deriving (Applicative, Functor, Monad, MonadError String, MonadFail, MonadIO)


-- | Lift in the Mantra monad.
mantraM :: Monad m
        => MonadTrans t
        => MantraM m a
        -> t (MantraM m) a
mantraM = lift


-- | Run the Mantra monad to IO.
runMantraToIO :: MantraM IO a
              -> IO (Either String a)
runMantraToIO = runExceptT . runMantraM


-- | Pull a value into the Mantra monad.
foistMantra :: Monad m
            => a           -- ^ The value.
            -> MantraM m a -- ^ Action for the value.
foistMantra = MantraM . ExceptT . return . Right


-- | Pull a value into the Mantra monad.
foistMantraEither :: Monad m
                  => Show e
                  => Either e a  -- ^ The value.
                  -> MantraM m a -- ^ Action for the value.
foistMantraEither = MantraM . withExceptT show . ExceptT . return


-- | Pull a value into the Mantra monad.
foistMantraEitherIO :: MonadIO m
                    => Show e
                    => IO (Either e a) -- ^ The value.
                    -> MantraM m a     -- ^ Action for the value.
foistMantraEitherIO = MantraM . hoistExceptT liftIO . withExceptT show . ExceptT


-- | Pull a value into the Mantra monad.
foistMantraExcept :: Monad m
                  => Show e
                  => ExceptT e m a -- ^ The value.
                  -> MantraM m a   -- ^ Action for the value.
foistMantraExcept = MantraM . withExceptT show


-- | Pull a value into the Mantra monad.
foistMantraExceptIO :: MonadIO m
                    => Show e
                    => ExceptT e IO a -- ^ The value.
                    -> MantraM m a    -- ^ Action for the value.
foistMantraExceptIO = MantraM . hoistExceptT liftIO . withExceptT show


-- | Pull a value into the Mantra monad.
foistMantraMaybe :: Monad m
                 => String      -- ^ The error message.
                 -> Maybe a     -- ^ The value.
                 -> MantraM m a -- ^ Action for the value.
foistMantraMaybe message = MantraM . ExceptT . return . maybe (Left message) Right


-- | Pull a value into the Mantra monad.
foistMantraMaybeIO :: MonadIO m
                   => String       -- ^ The error message.
                   -> IO (Maybe a) -- ^ The value.
                   -> MantraM m a  -- ^ Action for the value.
foistMantraMaybeIO message = MantraM . hoistExceptT liftIO . ExceptT . fmap (maybe (Left message) Right)


-- | Throw an error.
throwMantra :: Monad m
            => String      -- ^ The error message.
            -> MantraM m a -- ^ Action for throwing the error.
throwMantra = MantraM . throwE


-- | Log a message.
logMantra :: MonadIO m
          => Bool         -- ^ Whether to log to standard output.
          -> String       -- ^ The message.
          -> MantraM m () -- ^ Action to log the message.
logMantra out =
  liftIO
    . if out
        then putStrLn
        else hPutStrLn stderr


-- | Print a message to standard output.
printMantra :: MonadIO m
            => String       -- ^ The message.
            -> MantraM m () -- ^ Action to print the message.
printMantra = logMantra True


-- | Print a message to standard error.
debugMantra :: MonadIO m
            => String       -- ^ The message.
            -> MantraM m () -- ^ Action to print the message.
debugMantra = logMantra False


-- | A reference to a slot.
data SlotRef =
    AbsoluteSlot Integer -- ^ The absolute slot number.
  | RelativeSlot Integer -- ^ The number of slots relative to the current tip.
    deriving (Eq, Ord, Show)

instance Read SlotRef where
  readsPrec p ('+' : remainder) = first RelativeSlot <$> readsPrec p remainder
  readsPrec p        remainder  = first AbsoluteSlot <$> readsPrec p remainder
