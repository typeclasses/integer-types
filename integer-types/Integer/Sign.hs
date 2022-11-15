{-# language Safe #-}

module Integer.Sign
  (
    {- * Type -} Sign (..),
    {- * Operations -} negate, multiply,
  )
  where

import Prelude (Eq, Ord, Show, seq, (==))

import qualified Control.DeepSeq as DeepSeq

data Sign = MinusSign | PlusSign
    deriving (Eq, Ord, Show)

instance DeepSeq.NFData Sign where rnf x = seq x ()

negate :: Sign -> Sign
negate PlusSign  = MinusSign
negate MinusSign = PlusSign

multiply :: Sign -> Sign -> Sign
multiply a b = if a == b then PlusSign else MinusSign
