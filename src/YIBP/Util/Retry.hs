{-# LANGUAGE OverloadedStrings #-}

module YIBP.Util.Retry where

import YIBP.Logger

import Data.Bits

import Control.Concurrent
import Control.Exception
import Control.Monad

import Fmt

retry :: WithLogger => IO a -> IO a
retry act = loop 1
  where
    attemptsCount = 3 :: Int
    loop attemptNo = do
      when (attemptNo > attemptsCount) $ error "invalid state in retry: too many retries"
      try act >>= \case
        Right o -> pure o
        Left (err :: SomeException) | attemptNo == attemptsCount -> do
          logMsg Debug $ "Failed to perform action with error " +| show err |+ " after " +| attemptNo |+ " attempt. Giving up"
          throw err
        Left (err :: SomeException) -> do
          logMsg Debug $ "Failed to perform action with error " +| show err |+ " after " +| attemptNo |+ " attempt. Trying once again"
          threadDelay $ (1 `shiftL` attemptNo) * 1_000_000
          loop (attemptNo + 1)
