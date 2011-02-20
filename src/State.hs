{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
  MultiParamTypeClasses, TemplateHaskell, TypeFamilies, TypeSynonymInstances, 
  TypeOperators #-}

module State (
  AppState,
  
  -- Users
  Users, UsersMap,
  GetUsers(..), InsertUser(..)
  ) where

import State.Users

import Data.Data (Data, Typeable)

import Happstack.State (Component(..), End,Version, deriveSerialize, mkMethods, (:+:))

-- | State
data AppState = AppState
              deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = Users :+: End
  initialValue = AppState

$(mkMethods ''AppState [])