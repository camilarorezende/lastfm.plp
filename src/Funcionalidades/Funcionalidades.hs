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
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import GHC.Base (VecElem(Int16ElemRep))


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
  let contarScrobbles:: Usuario -> Int
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
  putStrLn "\nRanking global do LASTFM com base nos seus scrobbles! Os maiores ouvintes da nossa plataforma :) :"
  imprimeRank ranking
  where
    imprimeRank :: [(Usuario, Int)] -> IO ()
    imprimeRank [] = return ()
    imprimeRank ((usuario, qntsc):resto) = do
      putStrLn (nome usuario ++ " (" ++ email usuario ++ ") está com - Scrobbles: " ++ show qntsc ++ " ;)")
      imprimeRank resto    
--verConquistas ::

--recomendarMusicas ::

--verificarCompatibilidade ::



