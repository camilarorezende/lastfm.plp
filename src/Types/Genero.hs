module Types.Genero where

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
  deriving (Show, Eq, Enum, Bounded)
