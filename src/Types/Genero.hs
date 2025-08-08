{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Genero(Genero(..)) where

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
  deriving (Show, Eq, Ord, Enum, Read, Bounded, Generic, ToJSON, FromJSON)
