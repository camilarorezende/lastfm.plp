:- dynamic usuario/3.
:- dynamic musica/6.
:- dynamic scrobble/2.

main :-
  menu_principal.

menu_principal :-
  writeln(""),
  writeln("================= BEM-VINDO AO LASTFM ================="),
  writeln(""),
  writeln("1 - Cadastrar Novo Usuário"),
  writeln("2 - Fazer Login"),
  writeln("3 - Ver Estatísticas Globais"),
  writeln("4 - Ver Ranking Global"),
  writeln("5 - Sair"),
  prompt_string("\nEscolha uma opção: ", Opcao),
  ( Opcao = "1" ->
      prompt_string("\nDigite seu Nome: ", Nome),
      prompt_string("Digite seu Email: ", Email),
      prompt_string("Digite sua Senha: ", Senha),
      ( \+ valida_nome(Nome) ->
          writeln("\nNome inválido! Use apenas letras e espaços."),
          menu_principal
      ; \+ valida_email(Email) ->
          writeln("\nEmail inválido! Insira um email válido."),
          menu_principal
      ; existe_usuario_com_email(Email) ->
          writeln("\nJá existe um usuário cadastrado com esse email!"),
          menu_principal
      ; cadastrar_usuario(Nome, Email, Senha, _NovoUsuario),
        writeln("\nUsuário cadastrado com sucesso!"),
        menu_principal
      )
    ; Opcao = "2" ->
      prompt_string("\nDigite seu Email: ", EmailLogin),
      prompt_string("Digite sua Senha: ", SenhaLogin),
      ( login_usuario(EmailLogin, SenhaLogin, Usuario) ->
          format("\nBem-vindo(a) de volta, ~w!~n", [Usuario.nome]),
          menu_logado(Usuario)
      ; writeln("\nEmail ou senha incorretos. Tente novamente."),
        menu_principal
      )
    ; Opcao = "3" ->
      estatisticas_globais,
      menu_principal
    ; Opcao = "4" ->
      gerar_ranking_global,
      menu_principal
    ; Opcao = "5" ->
      writeln("Encerrando o programa. Até logo!")
    ; writeln("\nOpção inválida! Tente novamente."),
      menu_principal
  ).

menu_logado(Usuario) :-
  writeln(""),
  writeln("================= MENU USUÁRIO LOGADO ================="),
  writeln(""),
  format("Olá, ~w!~n", [Usuario.nome]),
  writeln(""),
  writeln("1 - Ver Perfil"),
  writeln("2 - Ver Conquistas"),
  writeln("3 - Registrar scrobble"),
  writeln("4 - Ver Ranking Pessoal"),
  writeln("5 - Receber Recomendação"),
  writeln("6 - Calcular Compatibilidade"),
  writeln("7 - Voltar ao Menu Principal"),
  prompt_string("\nEscolha uma opção: ", Opcao),
  ( Opcao = "1" ->
      writeln("\n=========== SEU PERFIL ============"),
      format("Nome: ~w~n", [Usuario.nome]),
      format("Email: ~w~n", [Usuario.email]),
      carregar_scrobbles(Scs),
      include(scrobble_do_email(Usuario.email), Scs, ScrobblesDoUsuario),
      historico_do_usuario(ScrobblesDoUsuario),
      menu_logado(Usuario)
    ; Opcao = "2" ->
      writeln("\n========= SUAS CONQUISTAS ========="),
      ver_conquistas(Usuario),
      menu_logado(Usuario)
    ; Opcao = "3" ->
      carregar_catalogo(Catalogo),
      ( Catalogo == [] ->
          writeln("Nenhuma música disponível no catálogo."),
          menu_logado(Usuario)
      ; writeln("\nEscolha uma música para scrobble:"),
        listar_catalogo(Catalogo, 1),
        prompt_string("\nDigite o número da música: ", Entrada),
        ( string_number_in_range(Entrada, 1, len(Catalogo), N) ->
            nth1(N, Catalogo, MusicaEscolhida),
            registrar_scrobble(Usuario, MusicaEscolhida, UsuarioAtualizado),
            menu_logado(UsuarioAtualizado)
        ; writeln("Entrada inválida. Tente novamente."),
          menu_logado(Usuario)
        )
      )
    ; Opcao = "4" ->
      gerar_ranking_pessoal(Usuario.email),
      menu_logado(Usuario)
    ; Opcao = "5" ->
      writeln("\nEscolha o tipo de recomendação:"),
      writeln("1 - Por gênero"),
      writeln("2 - Por artista"),
      writeln("3 - Baseada no histórico"),
      prompt_string("\nEscolha uma opção: ", TipoStr),
      ( string_number_in_set(TipoStr, [1,2,3], Tipo) ->
          ( Tipo = 1 ->
              Generos = ["Rock","Pop","Eletronica","HipHop","Rap","Funk","MPB","Sertanejo","Forro","Indie","Pagode"],
              writeln("\n--- GÊNEROS DISPONÍVEIS: ---"),
              listar_generos(Generos, 1),
              prompt_string("\nDigite o número do gênero: ", GStr),
              ( string_number_in_range(GStr, 1, len(Generos), GIdx) ->
                  nth1(GIdx, Generos, Param),
                  recomendar_musicas(Usuario, Tipo, Param, Musicas),
                  exibir_recomendacoes(Musicas),
                  menu_logado(Usuario)
              ; writeln("Gênero inválido. Tente novamente."),
                menu_logado(Usuario)
              )
            ; Tipo = 2 ->
              prompt_string("\nDigite o nome do artista: ", Param),
              recomendar_musicas(Usuario, Tipo, Param, Musicas),
              exibir_recomendacoes(Musicas),
              menu_logado(Usuario)
            ; Tipo = 3 ->
              recomendar_musicas(Usuario, Tipo, "", Musicas),
              exibir_recomendacoes(Musicas),
              menu_logado(Usuario)
          )
      ; writeln("Opção inválida!"),
        menu_logado(Usuario)
      )
    ; Opcao = "6" ->
      prompt_string("\nDigite o email do usuário para match: ", EmailOutro),
      carregar_usuarios(Usuarios),
      ( member(Outro, Usuarios), Outro.email = EmailOutro ->
          carregar_scrobbles(Scrobbles),
          verificar_compatibilidade(Usuario, Outro, Scrobbles, Score0a1),
          CompatInt is round(Score0a1*100),
          format("\nSeu match com ~w é de: >>>  ~w%  <<<~n", [Outro.nome, CompatInt]),
          ( CompatInt >= 80 ->
              writeln("\nQue match hein!? Ótimo para montarem uma playlist compartilhada!")
          ; CompatInt =< 50 ->
              writeln("\nXiii... talvez vocês devam descobrir algo em comum.")
          ; writeln("\nNada mau! Vejo bons interesses em comum!")
          )
      ; writeln("\nUsuário não encontrado! Tente novamente.")
      ),
      menu_logado(Usuario)
    ; Opcao = "7" ->
      writeln("Fazendo logout..."),
      menu_principal
    ; writeln("Opção inválida! Tente novamente."),
      menu_logado(Usuario)
  ).

listar_catalogo([], _).
listar_catalogo([musica(_, Titulo, Artista, Album, _, _)|R], I) :-
  format("~w - ~w - ~w - ~w~n", [I, Titulo, Artista, Album]),
  I2 is I+1,
  listar_catalogo(R, I2).

listar_generos([], _).
listar_generos([G|R], I) :-
  format("~w - ~w~n", [I, G]),
  I2 is I+1,
  listar_generos(R, I2).

exibir_recomendacoes([]) :-
  writeln("Nenhuma recomendação encontrado.").
exibir_recomendacoes(Musicas) :-
  writeln("\n===== Essas soam que nem você ====="),
  exibir_lista_musicas(Musicas).

exibir_lista_musicas([]).
exibir_lista_musicas([musica(_, Titulo, Artista, _, Genero, _)|R]) :-
  format("- ~w - ~w (~w)~n", [Titulo, Artista, Genero]),
  exibir_lista_musicas(R).

scrobble_do_email(Email, scrobble(EmailS, _)) :-
  Email = EmailS.

prompt_string(Msg, Str) :-
  write(Msg),
  flush_output,
  read_line_to_string(user_input, Str).

string_number_in_set(Str, Set, Num) :-
  catch(number_string(N, Str), _, fail),
  member(N, Set),
  Num = N.

string_number_in_range(Str, Min, Max, Num) :-
  catch(number_string(N, Str), _, fail),
  N >= Min,
  N =< Max,
  Num = N.

len(Lista, N) :- length(Lista, N).

existe_usuario_com_email(Email) :-
  carregar_usuarios(Usuarios),
  member(U, Usuarios),
  U.email = Email.

valida_nome(_).
valida_email(_).
carregar_usuarios(_).
cadastrar_usuario(_, _, _, _).
login_usuario(_, _, _).
estatisticas_globais.
gerar_ranking_global.
carregar_scrobbles(_).
historico_do_usuario(_).
ver_conquistas(_).
carregar_catalogo(_).
registrar_scrobble(_, _, _).
gerar_ranking_pessoal(_).
recomendar_musicas(_, _, _, _).
verificar_compatibilidade(_, _, _, _).
