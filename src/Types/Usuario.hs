Module Types.Usuario where

data Usuario = Usuario {
    nome :: String
    email :: String
    senha :: String
}  deriving (Show, Eq, Read)