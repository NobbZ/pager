{-|
Module      : Pager.Internal
Description : Defines functions, classes and types for internal use.
Copyright   : (c) Norbert Melzer, 2014
License     : MIT
Maintainer  : timmelzer@gmail.com
Stability   : experimental
Portability : portable

Functions, classes and types that are for internal use only
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Pager.Internal where

-- | Simple unboxing helping class, for internal use only
class Unbox a b where
  -- | Unboxes any value of type 'a' to type 'b'
  ub :: a -> b
