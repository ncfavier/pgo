{-# LANGUAGE RecordWildCards #-}
module Pack where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Pack a = Pack { bottom  :: Integer
                   , top     :: Integer
                   , objects :: Map Integer a
                   , names   :: Map String Integer
                   }

emptyAt :: Integer -> Pack a
emptyAt o = Pack o o M.empty M.empty

empty :: Pack a
empty = emptyAt 0

size :: Pack a -> Integer
size p = top p - bottom p

(!?) :: Pack a -> String -> Maybe (Integer, a)
Pack{..} !? n = do
    o <- M.lookup n names
    v <- M.lookup o objects
    return (o, v)

namedObjects :: Pack a -> [(String, a)]
namedObjects p = [ (n, v)
                 | (n, o) <- M.assocs (names p)
                 , Just v <- [M.lookup o (objects p)] ]

upwardsObjects :: Pack a -> [(Integer, a)]
upwardsObjects p = M.assocs (objects p)

downwardsObjects :: Pack a -> [(Integer, a)]
downwardsObjects p = reverse $ M.assocs (objects p)

pushUp :: Integer -> a -> Pack a -> Pack a
pushUp s v p = p { top = top p + s
                 , objects = M.insert (top p) v (objects p)
                 }

pushDown :: Integer -> a -> Pack a -> Pack a
pushDown s v p = p { bottom = o
                   , objects = M.insert o v (objects p)
                   } where o = bottom p - s

pushUpWithName :: String -> Integer -> a -> Pack a -> Maybe (Pack a)
pushUpWithName n s v p
    | n `M.member` names p = Nothing
    | otherwise = Just p { top = top p + s
                         , objects = M.insert (top p) v (objects p)
                         , names = M.insert n (top p) (names p)
                         }

pushDownWithName :: String -> Integer -> a -> Pack a -> Maybe (Pack a)
pushDownWithName n s v p
    | n `M.member` names p = Nothing
    | otherwise = Just p { bottom = o
                         , objects = M.insert o v (objects p)
                         , names = M.insert n o (names p)
                         } where o = bottom p - s

update :: String -> a -> Pack a -> Pack a
update n v p | Just offset <- M.lookup n (names p) =
    p { objects = M.insert offset v (objects p) }
