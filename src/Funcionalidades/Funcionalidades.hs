module Funcionalidades.Funcionalidades where

import Types.Musica
import Types.Scrobble
import Types.Usuario (Usuario(..))
import System.IO (hFlush, stdout)
import Data.Aeson (encodeFile, decodeFileStrict, encode)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Char (isLetter, isSpace)


scrobbleArquivo :: FilePath
scrobbleArquivo = "scrobbles.json"

salvarScrobbles :: [Scrobble] -> IO ()
salvarScrobbles scrobbles = B.writeFile scrobbleArquivo (encode scrobbles)

carregarScrobbles :: IO [Scrobble]
carregarScrobbles = do
  existe <- doesFileExist scrobbleArquivo
  if not existe
    then return []
    else do
      conteudo <- B.readFile scrobbleArquivo
      case decode conteudo of
        Just scs -> return scs
        Nothing  -> return []

usuariosArquivo :: FilePath
usuariosArquivo = "usuarios.json"

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

carregarCatalogo :: IO [Musica]
carregarCatalogo = do
  let catalogoArquivo = "catalogo.json"
  existe <- doesFileExist catalogoArquivo
  if not existe
    then return []
    else do
      mCatalogo <- decodeFileStrict catalogoArquivo
      case mCatalogo of
        Just catalogo -> return catalogo
        Nothing       -> return []

validaNome :: String -> Bool
validaNome nome = 
  not (null nome) && all (\c -> isLetter c || isSpace c) nome

validaEmail :: String -> Bool
validaEmail email = 
  let parts = splitOn '@' email
  in length parts == 2 && not (null (head parts)) && '.' `elem` (parts !! 1)
     && all (/= ' ') email
  where
    splitOn c s = case break (== c) s of
      (x, []) -> [x]
      (x, _:rest) -> x : splitOn c rest

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

    
registrarScrobble :: Usuario -> Musica -> IO ()
registrarScrobble usuario musicaEscolhida = do
  hora <- getCurrentTime
  let momento = formatTime defaultTimeLocale "%d/%m/%Y %H:%M" hora
      novoScrobble = Scrobble musicaEscolhida (email usuario) momento

  scs <- carregarScrobbles
  salvarScrobbles (novoScrobble : scs)

  putStrLn "Scrobble registrado com sucesso!"


historicoDoUsuario :: [Scrobble] -> IO ()
historicoDoUsuario scrobbles = 
  if null scrobbles
    then putStrLn "\nVocê ainda não tem scrobbles registrados."
    else do
      putStrLn "\nSeu histórico de scrobbles:"
      mapM_ (\s -> putStrLn $ "- " ++ titulo (musica s) ++ " - " ++ artista (musica s) ++ " (" ++ momento s ++ ")") scrobbles

--gerarRankingPessoal ::

--gerarRankingGlobal ::

--verConquistas ::

--recomendarMusicas ::

--verificarCompatibilidade ::


