{-# LANGUAGE BlockArguments #-}

module Funcionalidades.Funcionalidades where

import Types.Musica
import Types.Scrobble
import Types.Genero (Genero(..))
import Types.Usuario (Usuario(..))
import System.IO (hFlush, stdout)
import Data.Aeson (encodeFile, decodeFileStrict, encode)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Char (isLetter, isSpace)
import Funcionalidades.Conquistas 
import Data.List (nub)
import Control.Monad (unless)
import Data.List (find)
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

atualizarUsuario :: Usuario -> IO ()
atualizarUsuario usuarioAtualizado = do
  usuarios <- carregarUsuarios
  let usuariosAtualizados = map
        (\u -> if email u == email usuarioAtualizado then usuarioAtualizado else u)
        usuarios
  salvarUsuarios usuariosAtualizados


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

    
registrarScrobble :: Usuario -> Musica -> IO Usuario
registrarScrobble usuario musicaEscolhida = do
  hora <- getCurrentTime
  let momento = formatTime defaultTimeLocale "%d/%m/%Y %H:%M" hora
      novoScrobble = Scrobble musicaEscolhida (email usuario) momento

  scs <- carregarScrobbles
  let novosScrobbles = novoScrobble : scs
  salvarScrobbles novosScrobbles

  let scrobblesDoUsuario = filter (\s -> emailUsuario s == email usuario) novosScrobbles
  let conquistasPossiveis = getConquistasUsuario scrobblesDoUsuario
  let novasConquistas = filter (`notElem` conquistas usuario) conquistasPossiveis
  let conquistasAtualizadas = nub (conquistas usuario ++ novasConquistas)


  putStrLn "Scrobble registrado com sucesso!"
  unless (null novasConquistas) $ do
    putStrLn "\nParabéns! Você desbloqueou as seguintes conquistas:"
    mapM_ (putStrLn . ("- " ++)) novasConquistas

  let usuarioAtualizado = usuario { conquistas = conquistasAtualizadas }

  atualizarUsuario usuarioAtualizado

  return usuarioAtualizado


historicoDoUsuario :: [Scrobble] -> IO ()
historicoDoUsuario scrobbles = 
  if null scrobbles
    then putStrLn "\nVocê ainda não tem scrobbles registrados."
    else do
      putStrLn "\nSeu histórico de scrobbles:"
      mapM_ (\s -> putStrLn $ "- " ++ titulo (musica s) ++ " - " ++ artista (musica s) ++ " - " ++ album (musica s) ++ " (" ++ momento s ++ ")") scrobbles

gerarRankingPessoal :: Usuario -> IO ()
gerarRankingPessoal usuario = do
  scrobbles <- carregarScrobbles
  let scrobblesUsuario = [s | s <- scrobbles, emailUsuario s == email usuario]
  if null scrobblesUsuario
    then putStrLn "Você ainda não tem nenhum scrobble :( Que tal dar play em alguma música? ;)"
    else do
      let musicasOuvidas = map musica scrobblesUsuario
          contagem :: [(Musica, Int)]
          contagem = ordenaMusica musicasOuvidas

          ordenar:: [(Musica, Int)] -> [(Musica, Int)]
          ordenar [] = []
          ordenar (primeiro : resto) = inserir primeiro (ordenar resto)
            where
              inserir :: (Musica, Int) -> [(Musica, Int)] -> [(Musica, Int)]
              inserir (musica1, quantidadeVezes) [] = [(musica1, quantidadeVezes)]
              inserir (musica1, quantidadeVezes) ((musica, quantidade): restoLista)
                | quantidadeVezes >= quantidade = (musica1, quantidadeVezes) : (musica, quantidade) : restoLista
                | otherwise = (musica, quantidade) : inserir (musica1, quantidadeVezes) restoLista
          rank = ordenar contagem

      putStrLn "\nRanking das suas músicas mais escutadas!! Veja seus hits do momento: "
      printaORank rank 
  where
          printaORank [] = return ()
          printaORank ((musica, qnt) : resto) = do
            putStrLn (titulo musica ++ " - " ++ artista musica ++ " | Ouvidas: " ++ show qnt)
            printaORank resto

ordenaMusica:: [Musica] -> [(Musica, Int)]
ordenaMusica [] = []
ordenaMusica (musica1: musicasResto) =
    let resto = ordenaMusica musicasResto
        contarPrimeira musica1 [] = [(musica1, 1)]
        contarPrimeira musica1 ((musicaNaLista, quantasVezes): restoDaLista)
         |titulo musica1 == titulo musicaNaLista = (musicaNaLista, quantasVezes + 1) : restoDaLista
         |otherwise = (musicaNaLista, quantasVezes) : contarPrimeira musica1 restoDaLista
    in contarPrimeira musica1 resto

gerarRankingGlobal :: IO ()
gerarRankingGlobal = do
  usuarios <- carregarUsuarios
  scrobbles <- carregarScrobbles

  if null usuarios
    then putStrLn "\nNenhum usuário cadastrado ainda. Que tal ser o primeiro a se juntar ao LastFM? :)"
    else do
      let contarScrobbles :: Usuario -> Int
          contarScrobbles usuarioContandoSc = conta scrobbles 0
            where
              conta [] qntVezes = qntVezes
              conta (scrobble1 : restoDeScrobble) qntVezes =
                if emailUsuario scrobble1 == email usuarioContandoSc
                  then conta restoDeScrobble (qntVezes + 1)
                  else conta restoDeScrobble qntVezes

          rankingNaoOrdenado :: [(Usuario, Int)]
          rankingNaoOrdenado = [(usuarioAtual, contarScrobbles usuarioAtual) | usuarioAtual <- usuarios]

          ordenarSc :: [(Usuario, Int)] -> [(Usuario, Int)]
          ordenarSc [] = []
          ordenarSc (primeiroEl : restoSc) = inserir primeiroEl (ordenarSc restoSc)
            where
              inserir :: (Usuario, Int) -> [(Usuario, Int)] -> [(Usuario, Int)]
              inserir usuarioEScrobble [] = [usuarioEScrobble]
              inserir (usuario, qntSc) ((usuarioNaLista, qntLista): resto)
                | qntSc >= qntLista  = (usuario, qntSc) : (usuarioNaLista, qntLista) : resto
                | otherwise = (usuarioNaLista, qntLista) : inserir (usuario, qntSc) resto

          ranking = ordenarSc rankingNaoOrdenado

      putStrLn "\nRanking global do LASTFM com base nos seus scrobbles! Os maiores ouvintes da nossa plataforma:"
      imprimeRank ranking

  where
    imprimeRank :: [(Usuario, Int)] -> IO ()
    imprimeRank [] = return ()
    imprimeRank ((usuario, qntsc):resto) = do
      putStrLn (nome usuario ++ " (" ++ email usuario ++ ") está com " ++ show qntsc ++ " Scrobble(s)!")
      imprimeRank resto


verConquistas :: Usuario -> IO ()
verConquistas usuario = do
  let conquistasUsuario = conquistas usuario
  if null conquistasUsuario
    then putStrLn "\nVocê ainda não desbloqueou nenhuma conquista."
    else do
      putStrLn "\nConquistas desbloqueadas:"
      mapM_ (putStrLn . ("- " ++)) conquistasUsuario


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


