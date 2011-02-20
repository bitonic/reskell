module State.Users (
  Users
  ) where

import Happstack.Data.Serialize (Serialize(..), Version(..), Mode(..), safeGet,
                                 safePut, contain)
import Data.HashMap (HashMap)
import qualified Data.HashMap as M
import Data.Hashable (Hashable)
import Data.ByteString (ByteString)

-- | Users

instance Version (HashMap a b) where mode = Primitive
instance (Serialize a, Serialize b, Ord a, Hashable a) => Serialize (HashMap a b) where
    getCopy = contain $ fmap M.fromList safeGet
    putCopy = contain . safePut . M.toList

type Users = HashMap ByteString ByteString