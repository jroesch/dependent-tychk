module Hubris.Pretty where

import Hubris.Synatx

pretty :: (Show a) => Term a -> Pretty
pretty a = show a

