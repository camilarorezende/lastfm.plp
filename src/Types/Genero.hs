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
<<<<<<< Updated upstream
  | Pagode
  | Reggae
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)
=======
  | Pagode 
  deriving (Show, Eq, Enum, Read, Bounded, Generic, ToJSON, FromJSON)
>>>>>>> Stashed changes
