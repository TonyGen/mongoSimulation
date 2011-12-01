-- | An extensible record maps fields to values. Fields can be added to a record type by other modules. Records that don't have the value of a requested field return the field's default.

module Data.Record (
	module Data.Record,
	empty
) where

import Prelude hiding (lookup)
import GHC.Base (Any)
import Unsafe.Coerce
import Data.IntMap
import Data.Monoid (mempty)

type Record' = IntMap Any

class Record r where
	unwrap :: r -> Record'
	wrap :: Record' -> r -> r

type Field r v = (r -> v, (v -> v) -> r -> r)

fieldF :: (Record r) => Int -> (r -> v) -> Field r v
fieldF n missing = (at, up) where
	at r = maybe (missing r) unsafeCoerce $ lookup n (unwrap r)
	up f r = wrap (alter (Just . unsafeCoerce . f . maybe (missing r) unsafeCoerce) n (unwrap r)) r

fieldD n v = fieldF n (const v)

field n = fieldD n mempty
