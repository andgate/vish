{-# LANGUAGE RankNTypes #-}
module Vish.Application.Data.IORef.Lens where

import Control.Lens
import Control.Monad
import Data.IORef

infixr 4 @~
infixl 8 ^@


(^@) :: IORef s -> Getter s a -> IO a
(^@) r l =
  liftM (^.l) (readIORef r)
{-# INLINE (^@) #-}

(@~) :: Setter s s a b -> b -> IORef s -> IO ()
(@~) l v r =
  modifyIORef r $ l .~ v
{-# INLINE (@~) #-}
