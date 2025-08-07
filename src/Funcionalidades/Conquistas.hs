module Funcionalidades.Conquistas (
<<<<<<< HEAD
    conquistasDisponiveis,
    getConquistasUsuario,
    calcularConquistas,
    verConquistas
) where 
=======
  conquistasDisponiveis,
  getConquistasUsuario
) where
>>>>>>> f3cc254 (Salvando minhas alterações antes de mudar de branch)

import Types.Scrobble
import Types.Usuario
<<<<<<< HEAD
import Types.Scrobble
import Types.Musica

conquistasDisponiveis :: [String]
conquistasDisponiveis =
    [ "Primeiro Scrobble!"
    , "10 músicas ouvidas"
    , "Super Fã!"
    , "100 minutos escutados"
    ]
  
getConquistasUsuario :: Usuario -> [String]
getConquistasUsuario = conquistas

calcularConquistas :: Usuario -> [String]
calcularConquistas usuario =
    let scrobblesUsuario = scrobbles usuario
        totalScrobbles = length scrobblesUsuario
        totalMinutos = sum (map (duracao . musica) scrobblesUsuario)
        conquistasAtuais = conquistas usuario
        novasConquistas = [ c | (c, cond) <- regras totalScrobbles totalMinutos, cond, c `notElem` conquistasAtuais ]
    in novasConquistas

regras :: Int -> Int -> [(String, Bool)]
regras totalScrobbles totalMinutos =
    [ ("Primeiro Scrobble!", totalScrobbles >= 1)
    , ("10 músicas ouvidas", totalScrobbles >= 10)
    , ("Super Fã!", totalScrobbles >= 50)
    , ("100 minutos escutados", totalMinutos >= 100)
    ]

verConquistas :: Usuario -> IO ()
verConquistas usuario = do
    let conquistasUsuario = conquistas usuario
    if null conquistasUsuario
      then putStrLn "\nVocê ainda não desbloqueou nenhuma conquista."
      else do
        putStrLn "\nConquistas desbloqueadas:"
        mapM_ (\c -> putStrLn ("- " ++ c)) conquistasUsuario
=======
import Types.Musica
import Data.List (group, sort, maximumBy)

conquistasDisponiveis :: [String]
conquistasDisponiveis =
  [ "Primeiro Scrobble!"
  , "10 músicas ouvidas"
  , "Super Fã!"
  , "100 minutos escutados"
  ]

getConquistasUsuario :: Usuario -> [Scrobble] -> [String]
getConquistasUsuario usuario scrobblesUsuario =
  let
    totalScrobbles = length scrobblesUsuario
    totalMinutos = sum (map ((/ 60) . fromIntegral . duracao . musica) scrobblesUsuario)

    primeiroScrobble = if totalScrobbles >= 1 then ["Primeiro Scrobble!"] else []
    dezMusicas       = if totalScrobbles >= 10 then ["10 músicas ouvidas"] else []
    superFan         = if not (null scrobblesUsuario) &&
                          let artistaMaisOuvido = head . mostCommon $ map (artista . musica) scrobblesUsuario
                          in length (filter (\s -> artista (musica s) == artistaMaisOuvido) scrobblesUsuario) >= 5
                       then ["Super Fã!"]
                       else []
    cemMinutos       = if totalMinutos >= 100 then ["100 minutos escutados"] else []

  in primeiroScrobble ++ dezMusicas ++ superFan ++ cemMinutos

mostCommon :: Ord a => [a] -> [a]
mostCommon xs =
  let grouped = map (\l -> (head l, length l)) . group . sort $ xs
  in [fst $ maximumBy (\a b -> compare (snd a) (snd b)) grouped]
>>>>>>> f3cc254 (Salvando minhas alterações antes de mudar de branch)
