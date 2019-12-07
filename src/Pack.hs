module Pack where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Pack a = Pack { bottom  :: Integer
                   , top     :: Integer
                   , objects :: Map String (Integer, a)
                   }

emptyPack = Pack 0 0 M.empty

emptyPackOffset o = Pack o o M.empty

size p = top p - bottom p

getValue n p = M.lookup n (objects p)

pushUp n v s p | n `M.member` objects p = Nothing
               | otherwise = Just p { top = top p + s
                                    , objects = M.insert n (top p, v) (objects p)
                                    }

pushDown n v s p | n `M.member` objects p = Nothing
                 | otherwise = Just p { bottom = bottom'
                                      , objects = M.insert n (bottom', v) (objects p)
                                      }
                 where bottom' = bottom p - s
