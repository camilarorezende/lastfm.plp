{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Scrobble where

import Types.Usuario
import Types.Musica
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Scrobble = Scrobble {
    musica :: Musica,
    usuario :: Usuario,
    momento :: String --timestamp
}  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)