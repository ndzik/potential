{-# LANGUAGE NoImplicitPrelude #-}
module Potential.Test.Boundable where

import           Data.ByteString
import           Potential.Core
import           Prelude                        ( ($)
                                                , (+)
                                                , Eq(..)
                                                , Show(..)
                                                , fromIntegral
                                                )

newtype ContentMock = ContentMock ByteString deriving (Eq, Show)

instance Boundable ContentMock where
  boundsOf (ContentMock s) = Bounds 3 (2 + fromIntegral (length s))
