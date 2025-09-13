{-# LANGUAGE BlockArguments #-}

module Main where

import Funcionalidades.Funcionalidades
import Types.Usuario   (Usuario(..))
import Types.Musica    (Musica(..))
import Types.Scrobble  (Scrobble(..))
import System.IO       (hFlush, stdout)
import Data.List       (find)

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
  putStrLn "3 - Ver Estatísticas Globais"
  putStrLn "4 - Ver Ranking Global"
  putStrLn "5 - Sair"
  putStr "\nEscolha uma opção: "
  hFlush stdout
  opcao <- getLine
  case opcao of
    "1" -> do
      putStrLn "\nDigite seu Nome: "
      nomeInput <- getLine
      putStrLn "Digite seu Email: "
      emailInput <- getLine
      putStrLn "Digite sua Senha: "
      senhaInput <- getLine
      if not (validaNome nomeInput) then do
        putStrLn "\nNome inválido! Use apenas letras e espaços."
        menu usuarios
      else if not (validaEmail emailInput) then do
        putStrLn "\nEmail inválido! Insira um email válido."
        menu usuarios
      else if any (\u -> email u == emailInput) usuarios then do
        putStrLn "\nJá existe um usuário cadastrado com esse email!"
        menu usuarios
      else do
        let novoUsuario = Usuario nomeInput emailInput senhaInput []
        usuariosAtualizados <- cadastrarUsuario novoUsuario usuarios
        putStrLn "\nUsuário cadastrado com sucesso!"
        menu usuariosAtualizados

    "2" -> do
      putStrLn "\nDigite seu Email: "
      emailInput <- getLine
      putStrLn "Digite sua Senha: "
      senhaInput <- getLine
      resultado <- loginUsuario emailInput senhaInput usuarios
      case resultado of
        Just usuario -> do
          putStrLn ("\nBem-vindo(a) de volta, " ++ nome usuario ++ "!")
          menuLogado usuario
        Nothing -> do
          putStrLn "\nEmail ou senha incorretos. Tente novamente."
          menu usuarios

    "3" -> do
      estatisticasGlobais
      menu usuarios

    "4" -> do
      gerarRankingGlobal
      menu usuarios

    "5" -> do
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
  putStr "\nEscolha uma opção: "
  hFlush stdout
  opcao <- getLine
  case opcao of
    "1" -> do
      putStrLn "\n=========== SEU PERFIL ============"
      putStrLn ("Nome: " ++ nome usuario)
      putStrLn ("Email: " ++ email usuario)
      
      scs <- carregarScrobbles
      let scrobbles = filter (\s -> emailUsuario s == email usuario) scs
      historicoDoUsuario scrobbles
      menuLogado usuario

    "2" -> do
      putStrLn "\n========= SUAS CONQUISTAS ========="
      verConquistas usuario
      menuLogado usuario

    "3" -> do
      catalogo <- carregarCatalogo
      if null catalogo
        then putStrLn "Nenhuma música disponível no catálogo." >> menuLogado usuario
        else do
          putStrLn "\nEscolha uma música para scrobble:"
          mapM_ (\(i, m) -> putStrLn $ show i ++ " - " ++ titulo m ++ " - " ++ artista m ++ " - " ++ album m)
                (zip [1..] catalogo)
          putStr "\nDigite o número da música: "
          hFlush stdout
          entrada <- getLine
          case reads entrada of
            [(n, "")] | n > 0 && n <= length catalogo -> do
              let musicaEscolhida = catalogo !! (n - 1)
              usuarioAtualizado <- registrarScrobble usuario musicaEscolhida
              menuLogado usuarioAtualizado
            _ -> do
              putStrLn "Entrada inválida. Tente novamente."
              menuLogado usuario

    "4" -> do
      gerarRankingPessoal usuario
      menuLogado usuario

    "5" -> do
      putStrLn $ "\nEscolha o tipo de recomendação:\n" ++
                 "1 - Por gênero\n" ++
                 "2 - Por artista\n" ++
                 "3 - Baseada no histórico\n" ++
                 "\nEscolha uma opção: "
      tipoStr <- getLine
      case reads tipoStr of
        [(tipo, "")] | tipo `elem` [1,2,3] -> do
          parametro <- case tipo of
            1 -> do
              let generosDisponiveis = ["Rock","Pop","Eletronica","HipHop","Rap","Funk","MPB","Sertanejo","Forro","Indie","Pagode"]
              putStrLn "\n--- GÊNEROS DISPONÍVEIS: ---"
              mapM_ (\(i, g) -> putStrLn $ show i ++ " - " ++ g) (zip [1..] generosDisponiveis)
              putStr "\nDigite o número do gênero: "
              hFlush stdout
              generoIndexStr <- getLine
              case reads generoIndexStr of
                [(idx, "")] | idx > 0 && idx <= length generosDisponiveis ->
                  return (generosDisponiveis !! (idx - 1))
                _ -> do
                  putStrLn "Gênero inválido. Tente novamente."
                  return ""
            2 -> do
              putStrLn "\nDigite o nome do artista:"
              getLine
            3 -> return ""
            _ -> return ""
          musicas <- recomendarMusicas usuario tipo parametro
          if null musicas
            then putStrLn "Nenhuma recomendação encontrado."
            else do
              putStrLn "\n===== Essas soam que nem você ====="
              mapM_ (\m -> putStrLn $ "- " ++ titulo m ++ " - " ++ artista m ++ " (" ++ show (genero m) ++ ")") musicas
          menuLogado usuario
        _ -> do
          putStrLn "Opção inválida!"
          menuLogado usuario

    "6" -> do
      putStrLn "\nDigite o email do usuário para match: "
      emailOutro <- getLine
      usuarios <- carregarUsuarios
      case find (\u -> email u == emailOutro) usuarios of
        Just outro -> do
          scrobbles <- carregarScrobbles
          let compatibilidade = round (verificarCompatibilidade usuario outro scrobbles * 100) :: Int
          putStrLn $ "\nSeu match com " ++ nome outro ++ " é de: >>>  " ++ show compatibilidade ++ "%  <<<"
          if compatibilidade >= 80
            then putStrLn "\nQue match hein!? Ótimo para montarem uma playlist compartilhada!"
            else if compatibilidade <= 50
              then putStrLn "\nXiii... talvez vocês devam descobrir algo em comum."
              else putStrLn "\nNada mau! Vejo bons interesses em comum!"
        Nothing -> putStrLn "\nUsuário não encontrado! Tente novamente."
      menuLogado usuario

    "7" -> do
      putStrLn "Fazendo logout..."
      usuarios <- carregarUsuarios
      menu usuarios

    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      menuLogado usuario
