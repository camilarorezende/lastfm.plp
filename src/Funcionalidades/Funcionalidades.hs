module Funcionalidades where

import Types.BaseDeDados
import Types.Usuario
import System.IO (hFlush, stdout)
<<<<<<< Updated upstream

cadastrarUusario ::

loginUsuario ::

registrarScrobble ::

mostrarHistorico ::

gerarRankingPessoal ::

gerarRankingGlobal ::

verConquistas ::

recomendarMusicas ::

verificarCompatibilidade ::
=======
import Data.Aeson (encodeFile, decodeFileStrict, encode)
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
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

    
registrarScrobble :: Usuario -> Musica -> IO ()
registrarScrobble usuario musicaEscolhida = do
  hora <- getCurrentTime
  let momento = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" hora
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

gerarRankingPessoal :: Usuario -> IO ()
gerarRankingPessoal usuario = do
  scs <- carregarScrobbles
  let scrobblesUsuario = filter (\s -> emailUsuario s == email usuario) scs
  if null scrobblesUsuario
    then putStrLn "Você ainda não tem scrobbles registrados."
    else dos
      let musicas = map musica scrobblesUsuario
          agrupadas = groupBy ((==) `on` titulo) $ sortBy (comparing titulo) musicas
          contagens = map (\grp -> (head grp, length grp)) agrupadas
          ordenado = sortBy (flip (comparing snd)) contagens
      putStrLn "\nRanking pessoal - músicas mais escutadas:"
      mapM_ (\(m, c) -> putStrLn $ titulo m ++ " - " ++ artista m ++ " | Ouvidas: " ++ show c) ordenado


gerarRankingGlobal :: IO ()
gerarRankingGlobal = do
  usuarios <- carregarUsuarios
  scs <- carregarScrobbles
  let contarScrobbles u = length $ filter (\s -> emailUsuario s == email u) scs
      ranking = map (\u -> (u, contarScrobbles u)) usuarios
      rankingOrdenado = sortBy (flip (comparing snd)) ranking
  putStrLn "\nRanking global de usuários por scrobbles:"
  mapM_ (\(u, c) -> putStrLn $ nome u ++ " (" ++ email u ++ ") - Scrobbles: " ++ show c) rankingOrdenado


--verConquistas ::

--recomendarMusicas ::

--verificarCompatibilidade ::
>>>>>>> Stashed changes


