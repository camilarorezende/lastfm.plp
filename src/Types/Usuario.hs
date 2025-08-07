Module Types.Usuario where

<<<<<<< Updated upstream
=======
module Types.Usuario where

import Types.Scrobble ( Scrobble )
>>>>>>> Stashed changes
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Usuario = Usuario {
    nome :: String,
    email :: String,
    senha :: String,
    conquistas :: [String],
    scrobbles :: [Types.Scrobble.Scrobble]
}  deriving (Show, Eq, Read, Generic, Data.Aeson.ToJSON, Data.Aeson.FromJSON)