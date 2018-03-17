module Util where

import Debug.Trace

liftPredMaybe :: (a -> Bool) -> a -> Maybe a
liftPredMaybe p a
  | p a = Just a
  | otherwise = Nothing

debug = flip trace
