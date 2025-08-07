module Funcionalidades.Funcionalidades where

import Funcionalidades.Conquistas (getConquistasUsuario, conquistasDisponiveis)
import Types.Usuario (Usuario(..))
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

    
registrarScrobble :: Usuario -> Musica -> IO Usuario
registrarScrobble usuario musicaEscolhida = do
  hora <- getCurrentTime
  let momento = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" hora
      novoScrobble = Scrobble musicaEscolhida (email usuario) momento

  scs <- carregarScrobbles
  let novosScrobbles = novoScrobble : scs
  salvarScrobbles novosScrobbles

  let conquistasAntes = conquistas usuario
      conquistasDepois = getConquistasUsuario usuario novosScrobbles
      novasConquistas = filter (`notElem` conquistasAntes) conquistasDepois

  if null novasConquistas
    then putStrLn "Scrobble registrado com sucesso!"
    else do
      putStrLn "Scrobble registrado com sucesso!"
      putStrLn "Parabéns! Você desbloqueou as seguintes conquistas:"
      mapM_ (\c -> putStrLn ("- " ++ c)) novasConquistas

  let usuarioAtualizado = usuario { conquistas = conquistasDepois }
  return usuarioAtualizado



historicoDoUsuario :: [Scrobble] -> IO ()
historicoDoUsuario scrobbles = 
  if null scrobbles
    then putStrLn "\nVocê ainda não tem scrobbles registrados."
    else do
      putStrLn "\nSeu histórico de scrobbles:"
      mapM_ (\s -> putStrLn $ "- " ++ titulo (musica s) ++ " - " ++ artista (musica s) ++ " (" ++ momento s ++ ")") scrobbles

--gerarRankingPessoal ::

--gerarRankingGlobal ::

verConquistas :: Usuario -> [Scrobble] -> IO ()
verConquistas usuario scrobbles = do
  let conquistasUsuario = getConquistasUsuario usuario scrobbles
  if null conquistasUsuario
    then putStrLn "\nVocê ainda não desbloqueou nenhuma conquista."
    else do
      putStrLn "\nConquistas desbloqueadas:"
      mapM_ (\c -> putStrLn ("- " ++ c)) conquistasUsuario

--recomendarMusicas ::

--verificarCompatibilidade ::


