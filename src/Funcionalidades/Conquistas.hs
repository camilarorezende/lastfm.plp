module Funcionalidades.Conquistas (
    conquistasDisponiveis,
    getConquistasUsuario,
    calcularConquistas,
    verConquistas
) where 

import Types.Usuario
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