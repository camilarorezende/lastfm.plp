{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Musica where

import Types.Genero
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Musica = Musica {
    nome :: String,
    artista :: String,
    album :: String,
    genero :: Genero,
    duracao :: Int
} deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)