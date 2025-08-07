Module Types.Musica where

import Types.Genero
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Musica = Musica {
    titulo :: String,
    artista :: String,
    album :: String,
    genero :: Genero,
    duracao :: Int
} deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance FromJSON Musica
instance ToJSON Musica