module Integer.Sign
  (
    {- * Type -} Sign (..),
    {- * Operations -} negate, multiply,
  )
  where

import Essentials

import Prelude (seq)
import qualified Prelude as Enum (Enum (..))
import Data.Hashable (Hashable (hashWithSalt))

import qualified Control.DeepSeq as DeepSeq

data Sign = MinusSign | PlusSign
    deriving stock (Eq, Ord, Show, Enum, Bounded)

instance DeepSeq.NFData Sign where rnf x = seq x ()

instance Hashable Sign where
    hashWithSalt salt x = salt `hashWithSalt` (Enum.fromEnum x)

negate :: Sign -> Sign
negate PlusSign  = MinusSign
negate MinusSign = PlusSign

multiply :: Sign -> Sign -> Sign
multiply a b = if a == b then PlusSign else MinusSign
