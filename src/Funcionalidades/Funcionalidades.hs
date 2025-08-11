{-# LANGUAGE BlockArguments #-}

module Funcionalidades.Funcionalidades where

import Types.Musica    hiding (nome)        
import Types.Scrobble  (Scrobble(..))       
import Types.Genero    (Genero(..))
import Types.Usuario   (Usuario(..))        
import Funcionalidades.Conquistas
import Data.Time.LocalTime (getZonedTime)
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Data.Aeson (encodeFile, decodeFileStrict, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import System.Random.Shuffle (shuffleM)
import Control.Monad (unless)
import Data.Char (isLetter, isSpace)
import Data.List (group, sort, sortOn, groupBy, maximumBy, nub, intersect)
import Data.Function (on)
import Data.Ord (Down(..), comparing)

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
  | n <= 0            = return []
  | length lista <= n = return lista
  | otherwise = do
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
  hora <- getZonedTime
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

      let tempoTotal = sum [duracao (musica s) | s <- scrobbles]
      putStrLn $ "\nTempo total escutado: " ++ formatTempo tempoTotal
  where
    formatTempo :: Int -> String
    formatTempo totalSegundos =
      let (h, rest1) = totalSegundos `divMod` 3600
          (m, s) = rest1 `divMod` 60
      in show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

gerarRankingPessoal :: Usuario -> IO ()
gerarRankingPessoal usuario = do
  scrobbles <- carregarScrobbles
  let scrobblesUsuario = [s | s <- scrobbles, emailUsuario s == email usuario]
  if null scrobblesUsuario
    then putStrLn "Você ainda não tem nenhum scrobble :( Que tal dar play em alguma música? ;)"
    else do
      let musicasOuvidas = map musica scrobblesUsuario

          contagemMusicas :: [(Musica, Int)]
          contagemMusicas = ordenaMusica musicasOuvidas

          ordenar :: [(Musica, Int)] -> [(Musica, Int)]
          ordenar [] = []
          ordenar (primeiro : resto) = inserir primeiro (ordenar resto)
            where
              inserir :: (Musica, Int) -> [(Musica, Int)] -> [(Musica, Int)]
              inserir (musica1, quantidadeVezes) [] = [(musica1, quantidadeVezes)]
              inserir (musica1, quantidadeVezes) ((musica, quantidade): restoLista)
                | quantidadeVezes >= quantidade = (musica1, quantidadeVezes) : (musica, quantidade) : restoLista
                | otherwise = (musica, quantidade) : inserir (musica1, quantidadeVezes) restoLista
          rankMusicas = ordenar contagemMusicas

          generosOuvidos = map genero musicasOuvidas

          contagemGeneros :: [(Genero, Int)]
          contagemGeneros = ordenarGenero generosOuvidos

          ordenarGenero :: [Genero] -> [(Genero, Int)]
          ordenarGenero generos =
            let contagens = contarGeneros generos
            in ordenarPorFrequenciaDecrescente contagens

          contarGeneros :: [Genero] -> [(Genero, Int)]
          contarGeneros [] = []
          contarGeneros (generoAtual : demaisGeneros) =
            let contagensParciais = contarGeneros demaisGeneros
            in inserirOuIncrementar generoAtual contagensParciais

          inserirOuIncrementar :: Genero -> [(Genero, Int)] -> [(Genero, Int)]
          inserirOuIncrementar generoProcurado [] = [(generoProcurado, 1)]
          inserirOuIncrementar generoProcurado ((generoNaLista, qtd) : restoDaLista)
            | generoProcurado == generoNaLista = (generoNaLista, qtd + 1) : restoDaLista
            | otherwise = (generoNaLista, qtd) : inserirOuIncrementar generoProcurado restoDaLista

          ordenarPorFrequenciaDecrescente :: [(Genero, Int)] -> [(Genero, Int)]
          ordenarPorFrequenciaDecrescente [] = []
          ordenarPorFrequenciaDecrescente (parAtual : paresRestantes) =
            inserirEmOrdem parAtual (ordenarPorFrequenciaDecrescente paresRestantes)

          inserirEmOrdem :: (Genero, Int) -> [(Genero, Int)] -> [(Genero, Int)]
          inserirEmOrdem par [] = [par]
          inserirEmOrdem (generoA, qtdA) ((generoB, qtdB) : resto)
            | qtdA >= qtdB = (generoA, qtdA) : (generoB, qtdB) : resto
            | otherwise    = (generoB, qtdB) : inserirEmOrdem (generoA, qtdA) resto

      putStrLn "\nRanking das suas músicas mais escutadas! Veja seus hits do momento: "
      printaORank rankMusicas

      putStrLn "\nRanking dos gêneros mais ouvidos:"
      printaORankGeneros contagemGeneros
  where
    printaORank [] = return ()
    printaORank ((musica, qnt) : resto) = do
      putStrLn (titulo musica ++ " - " ++ artista musica ++ " | Ouvidas: " ++ show qnt)
      printaORank resto

    printaORankGeneros [] = return ()
    printaORankGeneros ((genero, qnt) : resto) = do
      putStrLn (show genero ++ " | Ouvidas: " ++ show qnt)
      printaORankGeneros resto

ordenaMusica :: [Musica] -> [(Musica, Int)]
ordenaMusica [] = []
ordenaMusica (musica1: musicasResto) =
    let resto = ordenaMusica musicasResto
        contarPrimeira musica1 [] = [(musica1, 1)]
        contarPrimeira musica1 ((musicaNaLista, quantasVezes): restoDaLista)
         | titulo musica1 == titulo musicaNaLista && artista musica1 == artista musicaNaLista
             = (musicaNaLista, quantasVezes + 1) : restoDaLista
         | otherwise = (musicaNaLista, quantasVezes) : contarPrimeira musica1 restoDaLista
    in contarPrimeira musica1 resto

gerarRankingGlobal :: IO ()
gerarRankingGlobal = do
  usuarios <- carregarUsuarios
  scrobbles <- carregarScrobbles

  if null usuarios
    then putStrLn "\nNenhum usuário cadastrado ainda. Que tal ser o primeiro a se juntar ao LastFM? :)"
    else do
      let
        scrobblesDoUsuario u = filter (\s -> emailUsuario s == email u) scrobbles
        contarScrobbles u = length (scrobblesDoUsuario u)
        tempoTotal u = sum [duracao (musica s) | s <- scrobblesDoUsuario u]

        topArtistas u = take 3 $ sortOn (Down . snd) $
          map (\grp -> (artista $ musica $ head grp, length grp)) $
          groupBy ((==) `on` (artista . musica)) $
          sortOn (artista . musica) (scrobblesDoUsuario u)

        topMusicas u = take 3 $ sortOn (Down . snd) $
          map (\grp -> (titulo $ musica $ head grp, length grp)) $
          groupBy ((==) `on` (titulo . musica)) $
          sortOn (titulo . musica) (scrobblesDoUsuario u)

        rankingNaoOrdenado :: [(Usuario, Int, Int, [(String, Int)], [(String, Int)])]
        rankingNaoOrdenado = [ (u, contarScrobbles u, tempoTotal u, topArtistas u, topMusicas u) | u <- usuarios ]

        ranking = sortOn (\(_, qnt, _, _, _) -> Down qnt) rankingNaoOrdenado

      putStrLn "\nRanking global do LASTFM baseado nos seus scrobbles! Os maiores ouvintes da nossa plataforma:\n"
      imprimeRank ranking
  where
    imprimeRank :: [(Usuario, Int, Int, [(String, Int)], [(String, Int)])] -> IO ()
    imprimeRank [] = return ()
    imprimeRank ((usuario, qntsc, tempo, artistas, musicas):resto) = do
      putStrLn $ nome usuario ++ " (" ++ email usuario ++ ")"
      putStrLn $ "  - Scrobbles: " ++ show qntsc
      putStrLn $ "  - Tempo total escutado: " ++ formatTempo tempo
      putStrLn $ "  - Top artistas:"
      mapM_ (\(artista, cnt) -> putStrLn $ "      * " ++ artista ++ " (" ++ show cnt ++ " scrobbles)") artistas
      putStrLn $ "  - Top músicas:"
      mapM_ (\(titulo, cnt) -> putStrLn $ "      * " ++ titulo ++ " (" ++ show cnt ++ " scrobbles)") musicas
      putStrLn ""
      imprimeRank resto

    formatTempo :: Int -> String
    formatTempo totalSegundos =
      let (h, rest1) = totalSegundos `divMod` 3600
          (m, s) = rest1 `divMod` 60
      in show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

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
  | genero == "Rock" = Just Rock
  | genero == "Pop" = Just Pop
  | genero == "Eletronica" = Just Eletronica
  | genero == "HipHop" = Just HipHop
  | genero == "Rap" = Just Rap
  | genero == "Funk" = Just Funk
  | genero == "MPB" = Just MPB
  | genero == "Sertanejo" = Just Sertanejo
  | genero == "Forro" = Just Forro
  | genero == "Indie" = Just Indie
  | genero == "Pagode" = Just Pagode
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

chaveMusica :: Musica -> (String, String)
chaveMusica m = (titulo m, artista m)

chavesJaOuvidas :: [Scrobble] -> [(String, String)]
chavesJaOuvidas scs = [ (titulo (musica s), artista (musica s)) | s <- scs ]

excluirJaOuvidas :: [Musica] -> [Scrobble] -> [Musica]
excluirJaOuvidas catalogo scsUser =
  let ouvi = chavesJaOuvidas scsUser
  in filter (\m -> chaveMusica m `notElem` ouvi) catalogo

topArtistasUsuario :: [Scrobble] -> [(String, Int)]
topArtistasUsuario scs =
  let arts = [ artista (musica s) | s <- scs ]
      grup = map (\g -> (head g, length g)) . group . sort $ arts
  in sortOn (Down . snd) grup

contagemGenerosUsuario :: [Scrobble] -> [(Genero, Int)]
contagemGenerosUsuario scs =
  let gs  = [ genero (musica s) | s <- scs ]
      grp = map (\g -> (head g, length g)) . group . sort $ gs
  in sortOn (Down . snd) grp

distribuirPesosGeneros :: [(Genero, Int)] -> [(Genero, Double)]
distribuirPesosGeneros [] = []
distribuirPesosGeneros [(g,_c)] = [(g,1.0)]
distribuirPesosGeneros lista@((gTop,cTop):(g2,c2):resto) =
  let totalD = fromIntegral (sum (map snd lista)) :: Double
      gap    = cTop - c2
      dom    = gap >= 10 || fromIntegral cTop / totalD >= 0.60
  in if dom
       then
         let pesoTop     = 0.80
             outros      = (g2,c2):resto
             somaOutrosD = fromIntegral (sum (map snd outros)) :: Double
             distribuirOutros os
               | null os        = []
               | somaOutrosD<=0 = let p = (1.0 - pesoTop) / fromIntegral (length os)
                                   in map (\(g,_) -> (g,p)) os
               | otherwise      = map (\(g,c) -> (g, (1.0 - pesoTop) * (fromIntegral c / somaOutrosD))) os
         in (gTop, pesoTop) : distribuirOutros outros
       else
         let propor (g,c) =
               let p = fromIntegral c / totalD
               in (g, max 0.05 p)       
             pesos0 = map propor lista
             soma   = sum (map snd pesos0)
         in map (\(g,p) -> (g, p / soma)) pesos0

selecionarPorDistribuicao :: Int -> [(Genero, Double)] -> [Musica] -> IO [Musica]
selecionarPorDistribuicao n dist catalogo =
  let seqGen =
        let reps (g,p) = replicate (max 1 (round (p * 100))) g
        in concatMap reps dist
  in do
    seqShuf <- shuffleM seqGen
    let go _ acc | length acc >= n = return acc
        go [] acc                  = return acc
        go (g:gs) acc =
          case [ m | m <- catalogo, genero m == g, m `notElem` acc ] of
            []    -> go gs acc
            (m:_) -> go gs (m:acc)
    go seqShuf []

recomendarMusicas :: Usuario -> Int -> String -> IO [Musica]
recomendarMusicas usuario opcao parametro = do
  catalogo <- carregarCatalogo
  scrobbles <- carregarScrobbles
  let historicoUsuario = filter (\s -> emailUsuario s == email usuario) scrobbles

  case opcao of
    1 -> case lerGenero parametro of
      Just g -> selecionarAleatorio (excluirJaOuvidas (filtrarPorGenero g catalogo) historicoUsuario) 3
      Nothing -> return []

    2 -> do
      let base = filtrarPorArtista parametro catalogo
          naoOuvidas = excluirJaOuvidas base historicoUsuario
      selecionarAleatorio naoOuvidas 3

    3 -> if null historicoUsuario then return []
           else recomendarAutomatica catalogo historicoUsuario

    _   -> return []

recomendarAutomatica :: [Musica] -> [Scrobble] -> IO [Musica]
recomendarAutomatica catalogo scrobblesUser = do
  let catalogoSemRepetidas = excluirJaOuvidas catalogo scrobblesUser
      distGeneros          = distribuirPesosGeneros (contagemGenerosUsuario scrobblesUser)

  if null distGeneros || null catalogoSemRepetidas
    then do
      putStrLn "Nenhuma música encontrada."
      return []
    else do
      putStrLn "\nRecomendando com base na distribuição dos seus gêneros:"
      mapM_ (\(g,p) -> putStrLn $ " - " ++ show g ++ ": " ++ show (round (p*100 :: Double)) ++ "%") distGeneros
      selecionarPorDistribuicao 3 distGeneros catalogoSemRepetidas

verificarCompatibilidade :: Usuario -> Usuario -> [Scrobble] -> Double
verificarCompatibilidade u1 u2 scrobbles =
  let
    scrobblesU1 = filter (\s -> emailUsuario s == email u1) scrobbles
    scrobblesU2 = filter (\s -> emailUsuario s == email u2) scrobbles

    generosU1   = nub [genero  (musica s) | s <- scrobblesU1]
    artistasU1  = nub [artista (musica s) | s <- scrobblesU1]
    generosU2   = nub [genero  (musica s) | s <- scrobblesU2]
    artistasU2  = nub [artista (musica s) | s <- scrobblesU2]

    generosComuns   = generosU1  `intersect` generosU2
    artistasComuns  = artistasU1 `intersect` artistasU2

    compatibilidadeGeneros =
      if null generosU1 then 0
      else fromIntegral (length generosComuns) / fromIntegral (length generosU1)

    compatibilidadeArtistas =
      if null artistasU1 then 0
      else fromIntegral (length artistasComuns) / fromIntegral (length artistasU1)

    compatibilidadeGeral = (compatibilidadeGeneros * 0.6) + (compatibilidadeArtistas * 0.4)
  in max 0 (min 1 compatibilidadeGeral)

formatTempo :: Int -> String
formatTempo totalSegundos =
  let (h, rest1) = totalSegundos `divMod` 3600
      (m, s)     = rest1 `divMod` 60
  in show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

estatisticasGlobais :: IO ()
estatisticasGlobais = do
  usuarios  <- carregarUsuarios
  scrobbles <- carregarScrobbles

  if null scrobbles
    then putStrLn "\nNenhum dado disponível para estatísticas globais ainda."
    else do
      let artistas = map (artista . musica) scrobbles
          (nomeArtTop, cntArtTop) = maxPorFrequencia artistas

          chavesMus = map (\s -> (titulo (musica s), artista (musica s))) scrobbles
          ((titTop, artTop), cntMusTop) = maxPorFrequencia chavesMus

          tempoTotal = sum [duracao (musica s) | s <- scrobbles]
          usuariosComScrobbles = length . filter (\u -> any ((== email u) . emailUsuario) scrobbles) $ usuarios
          tempoMedio = if usuariosComScrobbles > 0
                         then tempoTotal `div` usuariosComScrobbles
                         else 0

      putStrLn "\nEstatísticas Globais da Plataforma:"
      putStrLn $ "  Artista mais ouvido: " ++ nomeArtTop ++ " (" ++ show cntArtTop ++ " scrobbles)"
      putStrLn $ "  Música mais ouvida: " ++ titTop ++ " - " ++ artTop ++ " (" ++ show cntMusTop ++ " scrobbles)"
      putStrLn $ "  Tempo médio de escuta por usuário: " ++ formatTempo tempoMedio

maxPorFrequencia :: (Ord a) => [a] -> (a, Int)
maxPorFrequencia xs =
  let grupos = map (\g -> (head g, length g)) . group . sort $ xs
  in maximumBy (compare `on` snd) grupos

