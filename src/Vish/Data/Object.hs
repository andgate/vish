module Vish.Data.Object where

import Data.IORef

type Object a = IORef (Maybe a)
