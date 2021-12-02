module Utils where

import Prelude

import Data.Int (fromString)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
  
unsafeParseInt10 :: String -> Int
unsafeParseInt10 s = unsafePartial $ fromJust $ fromString $ s