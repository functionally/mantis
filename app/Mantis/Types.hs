
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Mantis.Types (
  MantisM(..)
, runMantisToIO
, foistMantis
, foistMantisEither
, foistMantisEitherIO
, foistMantisExcept
, foistMantisExceptIO
, foistMantisIO
, foistMantisMaybe
, foistMantisMaybeIO
, throwMantis
) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Control.Monad.Trans.Except.Extra (hoistExceptT)


newtype MantisM m a = MantisM {runMantisM :: ExceptT String m a}
  deriving (Applicative, Functor, Monad, MonadError String, MonadFail, MonadIO) 


runMantisToIO :: MantisM IO a
              -> IO (Either String a)
runMantisToIO = runExceptT . runMantisM


foistMantisExcept :: Monad m
                  => Show e
                  => ExceptT e m a
                  -> MantisM m a
foistMantisExcept = MantisM . withExceptT show


foistMantisExceptIO :: MonadIO m
                    => Show e
                    => ExceptT e IO a
                    -> MantisM m a
foistMantisExceptIO = MantisM . hoistExceptT liftIO . withExceptT show


foistMantisEitherIO :: MonadIO m
                    => Show e
                    => IO (Either e a)
                    -> MantisM m a
foistMantisEitherIO = MantisM . hoistExceptT liftIO . withExceptT show . ExceptT


foistMantisMaybeIO :: MonadIO m
                   => String
                   -> IO (Maybe a)
                   -> MantisM m a
foistMantisMaybeIO message = MantisM . hoistExceptT liftIO . ExceptT . fmap (maybe (Left message) Right)


foistMantisMaybe :: Monad m
                 => String
                 -> Maybe a
                 -> MantisM m a
foistMantisMaybe message = MantisM . ExceptT . return . maybe (Left message) Right


foistMantisEither :: Monad m
                  => Show e
                  => Either e a
                  -> MantisM m a
foistMantisEither = MantisM . withExceptT show . ExceptT . return


foistMantis :: Monad m
            => a
            -> MantisM m a
foistMantis = MantisM . ExceptT . return . Right


foistMantisIO :: MonadIO m
              => IO a
              -> MantisM m a
foistMantisIO = liftIO
-- FIXME: replace with liftIO

throwMantis :: Monad m
            => String
            -> MantisM m a
throwMantis = MantisM . throwE
