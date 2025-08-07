module Main where

import Funcionalidades.Funcionalidades 
import Types.Usuario (Usuario(..))
import Types.Musica
import Types.Scrobble
import System.IO (hFlush, stdout)


main :: IO ()
main = do
  usuarios <- carregarUsuarios
  menu usuarios

menu :: [Usuario] -> IO ()
menu usuarios = do
    putStrLn "\n================= BEM-VINDO AO LASTFM ================="
    putStrLn ""
    putStrLn "1 - Cadastrar Novo Usuário"
    putStrLn "2 - Fazer Login"
    putStrLn "3 - Ver Ranking Global"
    putStrLn "4 - Sair"
    putStrLn "\nEscolha uma opçâo: "
    opcao <- getLine 
    case opcao of
      "1" -> do
         putStrLn "\nDigite seu Nome: "
         nome <- getLine
         putStrLn "Digite seu Email: "
         email <- getLine
         putStrLn "Digite sua Senha: "
         senha <- getLine

         let existeEmail = any (\u -> email == Types.Usuario.email u) usuarios
         if existeEmail
           then do
             putStrLn "\nJá existe um usuário cadastrado com esse email!"
             menu usuarios
           else do
             let novoUsuario = Usuario nome email senha []
             usuariosAtualizados <- cadastrarUsuario novoUsuario usuarios
             putStrLn "\nUsuário cadastrado com sucesso!"
             menu usuariosAtualizados

      "2" -> do
         putStrLn "\nDigite seu Email: "
         email <- getLine
         putStrLn "Digite sua Senha: "
         senha <- getLine
         resultado <- loginUsuario email senha usuarios
         case resultado of
            Just usuario -> do
               putStrLn ("\nBem-vindo(a) de volta, " ++ nome usuario ++ "!")    
               menuLogado usuario

            Nothing -> do
               putStrLn "\nEmail ou senha incorretos. Tente novamente."
               menu usuarios

      "3" -> do
         menu usuarios
        
      "4" -> do
         putStrLn "Encerrando o programa. Até logo!"
         return ()

      _ -> do
         putStrLn "\nOpção inválida! Tente novamente."
         menu usuarios


menuLogado :: Usuario -> IO ()
menuLogado usuario = do
  putStrLn "\n================= MENU USUÁRIO LOGADO ================="
  putStrLn ""
  putStrLn ("Olá, " ++ nome usuario ++ "!")
  putStrLn ""
  putStrLn "1 - Ver Perfil"
  putStrLn "2 - Ver Conquistas"
  putStrLn "3 - Registrar scrobble"
  putStrLn "4 - Ver Ranking Pessoal"
  putStrLn "5 - Receber Recomendação" 
  putStrLn "6 - Calcular Compatibilidade"
  putStrLn "7 - Voltar ao Menu Principal"
  putStrLn "\nEscolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      putStrLn "\n=========== SEU PERFIL ============"
      putStrLn ("Nome: " ++ nome usuario)
      putStrLn ("Email: " ++ email usuario)
      putStrLn ("Conquistas: " ++ show (conquistas usuario))
      scs <- carregarScrobbles         
      let scrobbles = filter (\s -> emailUsuario s == email usuario) scs
      historicoDoUsuario scrobbles           
      menuLogado usuario

    "2" -> do
      
      menuLogado usuario

    "3" -> do
      catalogo <- carregarCatalogo
      if null catalogo
        then putStrLn "Nenhuma música disponível no catálogo." >> menuLogado usuario
        else do
          putStrLn "\nEscolha uma música para scrobble:"
          mapM_ (\(i, m) -> putStrLn $ show i ++ " - " ++ titulo m ++ " - " ++ artista m)
                (zip [1..] catalogo)
          putStr "\nDigite o número da música: "
          hFlush stdout
          entrada <- getLine
          case reads entrada of
            [(n, "")] | n > 0 && n <= length catalogo -> do
              let musicaEscolhida = catalogo !! (n - 1)
              registrarScrobble usuario musicaEscolhida
              menuLogado usuario
            _ -> do
              putStrLn "Entrada inválida. Tente novamente."
              menuLogado usuario

    "4" -> do
     
      menuLogado usuario

    "5" -> do

      menuLogado usuario

    "6" -> do

      menuLogado usuario

    "7" -> do
      putStrLn "Fazendo logout..."
      usuarios <- carregarUsuarios
      menu usuarios

    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      menuLogado usuario
