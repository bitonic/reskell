{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module DB.State (
  
    AppState
    ) where


import Data.Data               (Data, Typeable)

import Happstack.State

data AppState = AppState
              deriving (Eq, Ord, Read, Show, Data, Typeable)
instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState

$(mkMethods ''AppState [])