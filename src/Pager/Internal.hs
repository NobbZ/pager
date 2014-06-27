{-# LANGUAGE MultiParamTypeClasses #-}

module Pager.Internal where

class Unbox a b where
  ub :: a -> b
