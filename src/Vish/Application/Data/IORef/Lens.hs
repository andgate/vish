{-# LANGUAGE RankNTypes #-}
module Vish.Application.Data.IORef.Lens where

import Control.Lens
import Control.Monad
import Data.IORef

infixr 4 @~
infixr 4 @%~
infixl 8 ^@


(^@) :: IORef s -> Getter s a -> IO a
(^@) r l =
  liftM (^.l) (readIORef r)
{-# INLINE (^@) #-}

(@~) :: Setter s s a a -> a -> IORef s -> IO ()
(@~) l x r =
  modifyIORef r $ l .~ x
{-# INLINE (@~) #-}

(@%~) :: Setter s s a a -> (a -> a) -> IORef s -> IO ()
(@%~) l f r =
  modifyIORef r $ l %~ f
{-# INLINE (@%~) #-}
