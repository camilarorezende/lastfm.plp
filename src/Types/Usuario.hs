Module Types.Usuario where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Usuario = Usuario {
    nome :: String,
    email :: String,
    senha :: String,
    conquistas :: [String]
}  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)