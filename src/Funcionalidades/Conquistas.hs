module Funcionalidades.Conquistas (
  conquistasDisponiveis,
  getConquistasUsuario 
) where

import Types.Usuario
import Types.Scrobble
import Types.Musica

conquistasDisponiveis :: [String]
conquistasDisponiveis = 
    [  "Primeiro Scrobble!", 
       "10 músicas ouvidas", 
       "Super Fã!", 
       "100 minutos escutados"  
    ]

getConquistasUsuario :: [Scrobble] -> [String]
getConquistasUsuario scrobblesUsuario =
  let
    totalScrobbles = length scrobblesUsuario
    totalMinutosDouble = sum (map (fromIntegral . duracao . musica) scrobblesUsuario) / 60
    totalMinutos = floor totalMinutosDouble
    novasConquistas = [ c | (c, cond) <- regras totalScrobbles totalMinutos, cond ]
  in novasConquistas



regras :: Int -> Int -> [(String, Bool)]
regras totalScrobbles totalMinutos =
  [ ("Primeiro Scrobble!", totalScrobbles >= 1)
  , ("10 músicas ouvidas", totalScrobbles >= 10)
  , ("Super Fã!", totalScrobbles >= 50)
  , ("100 minutos escutados", totalMinutos >= 100)
  ]
