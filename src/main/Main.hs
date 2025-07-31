module Main where

import Funcionalidades.Funcionalidades 
import Types.Usuario (Usuario(..))

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
  putStrLn "4 - Calcular Compatibilidade"
  putStrLn "5 - Voltar ao Menu Principal"
  putStrLn "\nEscolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      putStrLn ("Nome: " ++ nome usuario)
      putStrLn ("Email: " ++ email usuario)
      putStrLn ("Conquistas: " ++ show (conquistas usuario))
      menuLogado usuario

    "2" -> do
      
      menuLogado usuario

    "3" -> do
     
      menuLogado usuario

    "4" ->

      menuLogado usuario

    "5" -> do
      putStrLn "Fazendo logout..."
      usuarios <- carregarUsuarios
      menu usuarios

    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      menuLogado usuario
