module Types.Genero where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Genero
  = Rock
  | Pop
  | Eletronica
  | Classico
  | HipHop
  | Rap
  | Funk
  | MPB
  | Sertanejo
  | Forro
  | Indie
  | Pagode
  | Reggae
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)
