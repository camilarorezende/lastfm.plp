module Funcionalidades.Funcionalidades where

import Types.Musica
import Types.Scrobble
import Types.Usuario (Usuario(..))
import Types.Genero (Genero(..))
import System.IO (hFlush, stdout, putStrLn, getLine)
import Data.Aeson (encodeFile, decodeFileStrict, encode, decode)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Char (isLetter, isSpace)
import Data.List (nub, intersect, intercalate, sortOn, group, sort, length, maximumBy)
import Data.List (find)
import System.Random.Shuffle (shuffleM)
import Data.Ord (comparing)

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

selecionarAleatorio :: [a] -> Int -> IO [a]
selecionarAleatorio lista n
  |length lista <= n = return lista
  |otherwise = do
    listaEmbaralhada <- shuffleM lista
    return (take n listaEmbaralhada)

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

lerGenero :: String -> Maybe Genero
lerGenero genero 
  | genero== "Rock" = Just Rock
  | genero== "Pop" = Just Pop
  | genero== "Eletronica" = Just Eletronica
  | genero== "HipHop" = Just HipHop
  | genero== "Rap" = Just Rap
  | genero== "Funk" = Just Funk
  | genero=="MPB" = Just MPB
  | genero== "Sertanejo" = Just Sertanejo
  | genero== "Forro" = Just Forro
  | genero== "Indie" = Just Indie
  | genero== "Pagode" = Just Pagode
  | otherwise = Nothing

filtrarPorGenero :: Genero -> [Musica] -> [Musica]
filtrarPorGenero generoDesejado = filter (\m -> genero m == generoDesejado)

filtrarPorArtista :: String -> [Musica] -> [Musica]
filtrarPorArtista nomeArtista = filter (\m -> artista m == nomeArtista)

generoMaisOuvido :: [Scrobble] -> Genero
generoMaisOuvido scrobbles =
  let generos = map (genero . musica) scrobbles
      contagem = map (\g -> (head g, length g)) . group . sort $ generos
  in fst (maximumBy (comparing snd) contagem)

recomendarMusicas :: Usuario -> Int -> String -> IO[Musica]
recomendarMusicas usuario opcao parametro = do
  catalogo <- carregarCatalogo
  scrobbles <- carregarScrobbles
  let historicoUsuario = filter (\s -> emailUsuario s == email usuario) scrobbles

  case opcao of
    1 -> case lerGenero parametro of 
      Just g -> selecionarAleatorio(filtrarPorGenero g catalogo) 3
      Nothing -> return[]
    2 -> selecionarAleatorio(filtrarPorArtista parametro catalogo) 1
    3 -> if null historicoUsuario then return[]
              else recomendarAutomatica catalogo historicoUsuario
    _   -> return[]

recomendarAutomatica :: [Musica] -> [Scrobble] -> IO [Musica]
recomendarAutomatica catalogo scrobbles = do
  let generoFav = generoMaisOuvido scrobbles
      musicasDoGenero = filtrarPorGenero generoFav catalogo
  putStrLn $ "\nRecomendando baseado no seu gênero mais ouvido: " ++ show generoFav
  if null musicasDoGenero
    then do
      putStrLn "Nenhuma música encontrada."
      return []
      else selecionarAleatorio musicasDoGenero 3


verificarCompatibilidade :: Usuario -> Usuario -> [Scrobble] -> Double
verificarCompatibilidade u1 u2 scrobbles = do
    let
      scrobblesU1 = filter (\s -> emailUsuario s == email u1) scrobbles
      scrobblesU2 = filter (\s -> emailUsuario s == email u2) scrobbles
        
      generosU1 = nub [genero (musica s) | s <- scrobblesU1]
      artistasU1 = nub [artista (musica s) | s <- scrobblesU1]

      generosU2 = nub [genero (musica s) | s <- scrobblesU2]
      artistasU2 = nub [artista (musica s) | s <- scrobblesU2]

      generosComuns = generosU1 `intersect` generosU2
      artistasComuns = artistasU1 `intersect` artistasU2

      compatibilidadeGeneros = if null generosU1 then 0
        else fromIntegral (length generosComuns) / fromIntegral (length generosU1)

      compatibilidadeArtistas = if null artistasU1 then 0
        else fromIntegral (length artistasComuns) / fromIntegral(length artistasU1)

      compatibilidadeGeral = (compatibilidadeGeneros * 0.6) + (compatibilidadeArtistas *0.4)
      in max 0 (min 1 compatibilidadeGeral)