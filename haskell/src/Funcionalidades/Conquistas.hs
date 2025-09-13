module Funcionalidades.Conquistas
  ( conquistasDisponiveis
  , getConquistasUsuario
  ) where

import qualified Types.Scrobble as S
import qualified Types.Musica   as M
import Data.List (group, sort)

conquistasDisponiveis :: [String]
conquistasDisponiveis =
  [ "Primeiro Scrobble!"
  , "10 músicas ouvidas"
  , "100 minutos escutados"
  ]

getConquistasUsuario :: [S.Scrobble] -> [String]
getConquistasUsuario scrobblesUsuario =
  let
    totalScrobbles :: Int
    totalScrobbles = length scrobblesUsuario

    totalMinutos :: Int
    totalMinutos = sum (map (M.duracao . S.musica) scrobblesUsuario) `div` 60

    conquistasFixas :: [String]
    conquistasFixas =
      [ c | (c, cond) <- regras totalScrobbles totalMinutos, cond ]

    artistas :: [String]
    artistas = map (M.artista . S.musica) scrobblesUsuario

    superFaArtistas :: [String]
    superFaArtistas =
      [ "Super Fã de " ++ a
      | (a, n) <- groupCount artistas
      , n >= 50
      ]

    musicasChave :: [(String, String)]
    musicasChave =
      [ (M.titulo m, M.artista m)
      | s <- scrobblesUsuario
      , let m = S.musica s
      ]

    superFaMusicas :: [String]
    superFaMusicas =
      [ "Super Fã da música " ++ t ++ " - " ++ a
      | ((t,a), n) <- groupCount musicasChave
      , n >= 50
      ]
  in
    conquistasFixas ++ superFaArtistas ++ superFaMusicas

regras :: Int -> Int -> [(String, Bool)]
regras totalScrobbles totalMinutos =
  [ ("Primeiro Scrobble!", totalScrobbles >= 1)
  , ("10 músicas ouvidas", totalScrobbles >= 10)
  , ("100 minutos escutados", totalMinutos >= 100)
  ]

groupCount :: Ord a => [a] -> [(a, Int)]
groupCount xs = map (\g -> (head g, length g)) . group . sort $ xs
