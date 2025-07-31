module Funcionalidades.Funcionalidades where

import Types.BaseDeDados
import Types.Usuario (Usuario(..))
import System.IO (hFlush, stdout)
import Data.Aeson (encodeFile, decodeFileStrict)
import System.Directory (doesFileExist)


usuariosArquivo :: FilePath
usuariosArquivo = "dados.json"

salvarUsuarios :: [Usuario] -> IO ()
salvarUsuarios usuarios = encodeFile usuariosArquivo usuarios

carregarUsuarios :: IO [Usuario]
carregarUsuarios = do
  existe <- doesFileExist usuariosArquivo
  if existe
    then do
      mUsuarios <- decodeFileStrict usuariosArquivo
      case mUsuarios of
        Just usuarios -> return usuarios
        Nothing -> return []
    else return []

cadastrarUsuario :: Usuario -> [Usuario] -> IO [Usuario]
cadastrarUsuario novoUsuario usuarios = do
  let usuarioComConquistas = novoUsuario { conquistas = [] }
      usuariosAtualizados = usuarioComConquistas : usuarios
  salvarUsuarios usuariosAtualizados
  return usuariosAtualizados

loginUsuario :: String -> String -> [Usuario] -> IO (Maybe Usuario)
loginUsuario emailInput senhaInput usuarios = do
  let usuarioEncontrado = 
        filter (\u -> email u == emailInput && senha u == senhaInput) usuarios
  case usuarioEncontrado of
    (u:_) -> return (Just u)
    []    -> return Nothing

    
--registrarScrobble ::

--mostrarHistorico ::

--gerarRankingPessoal ::

--gerarRankingGlobal ::

--verConquistas ::

--recomendarMusicas ::

--verificarCompatibilidade ::


