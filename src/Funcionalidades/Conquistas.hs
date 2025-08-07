module Conquistas (
  conquistasDisponiveis,
  getConquistasUsuario  
) where

import Types.Usuario

conquistasDisponiveis :: [String]
conquistasDisponiveis = 
    [  "Primeiro Scrobble!", 
       "10 músicas ouvidas", 
       "Super Fã!", 
       "100 minutos escutados"  
    ]

getConquistasUsuario ::