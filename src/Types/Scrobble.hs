Module Types.Scrobble where

import Types.Usuario
import Types.Musica

data Scrobble = Scrobble {
    musica :: Musica
    usuario :: Usuario
    momento :: String --timestamp
}  deriving (Show, Eq, Read)