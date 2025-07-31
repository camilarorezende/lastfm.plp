{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types.BaseDeDados where

import Types.Usuario
import Types.Musica
import Types.Scrobble
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data BaseDeDados = BaseDeDados {
    usuarios :: [Usuario],
    musicas :: [Musica],
    scrobbles :: [Scrobble]
} deriving (Show, Generic, ToJSON, FromJSON)