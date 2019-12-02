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

getOffset n p = fst <$> M.lookup n (objects p)
getValue n p  = snd <$> M.lookup n (objects p)

pushUp n v s p =
    if n `M.member` objects p then Nothing else Just $
    p { top = top p + s
      , objects = M.insert n (top p, v) (objects p)
      }

pushDown n v s p =
    if n `M.member` objects p then Nothing else Just $
    let bottom' = bottom p - s in
    p { bottom = bottom'
      , objects = M.insert n (bottom', v) (objects p)
      }
