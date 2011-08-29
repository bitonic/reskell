{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Common () where

import Data.Data (Typeable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.SafeCopy

import Happstack.Data.IxSet (IxSet, Indexable)
import qualified Happstack.Data.IxSet as Ix

instance (SafeCopy a, Ord a, Indexable a, Typeable a) => SafeCopy (IxSet a) where
  getCopy = contain $ fmap Ix.fromList safeGet
  putCopy = contain . safePut . Ix.toList

instance (Eq a, Hashable a, SafeCopy a, SafeCopy b) => SafeCopy (HashMap a b) where
  getCopy = contain $ fmap HM.fromList safeGet
  putCopy = contain . safePut . HM.toList
  