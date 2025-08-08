{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Genero where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Genero
  = Rock
  | Pop
  | Eletronica
  | HipHop
  | Rap
  | Funk
  | MPB
  | Sertanejo
  | Forro
  | Indie
  | Pagode 
  deriving (Show, Eq, Enum, Read, Bounded, Generic, ToJSON, FromJSON, Ord)
