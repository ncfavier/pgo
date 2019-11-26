{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Transformers where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

instance {-# OVERLAPPABLE #-} MonadReader r m => MonadReader r (ReaderT r' m) where
    ask = lift ask
    local = mapReaderT . local

instance {-# OVERLAPPABLE #-} (Monoid w', MonadWriter w m) => MonadWriter w (WriterT w' m) where
    writer = lift . writer
    tell = lift . tell
    listen = undefined
    pass = undefined

instance {-# OVERLAPPABLE #-} MonadState s m => MonadState s (StateT s' m) where
    get = lift get
    put = lift . put
