module Pack where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Pack a = Pack { bottom  :: Integer
                   , top     :: Integer
                   , objects :: Map Integer a
                   , names   :: Map String Integer
                   }

emptyPackAt :: Integer -> Pack a
emptyPackAt o = Pack o o M.empty M.empty

emptyPack :: Pack a
emptyPack = emptyPackAt 0

size :: Pack a -> Integer
size p = top p - bottom p

nameAssocs :: Pack a -> [(String, a)]
nameAssocs p = [(n, v) | (n, o) <- M.assocs (names p), Just v <- [M.lookup o (objects p)]]

getOffsetByName :: String -> Pack a -> Maybe Integer
getOffsetByName n p = M.lookup n (names p)

getValueByName :: String -> Pack a -> Maybe a
getValueByName n p = M.lookup (names p M.! n) (objects p)

getOffsetAndValueByName :: String -> Pack a -> Maybe (Integer, a)
getOffsetAndValueByName n p = do
    offset <- M.lookup n (names p)
    value <- M.lookup offset (objects p)
    return (offset, value)

updateByName :: String -> a -> Pack a -> Pack a
updateByName n v p | Just offset <- M.lookup n (names p) =
    p { objects = M.insert offset v (objects p) }

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

traverseDownwards f p = traverse_ (\(o, v) -> f o v) $ reverse $ M.assocs (objects p)

upwardsAssocs p = M.assocs (objects p)
downwardsAssocs p = reverse $ M.assocs (objects p)
