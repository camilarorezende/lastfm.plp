:- set_prolog_flag(encoding, utf8).
:- module(main, [main/0]).

:- use_module('../Funcionalidades/funcionalidades').
:- use_module('../Types/genero').

main :-
    writeln('\nInicializando o sistema...'),
    carregar_dados_iniciais(Usuarios),
    writeln('Dados carregados com sucesso.'),
    menu(Usuarios).

menu(Usuarios) :-
    writeln('\n================= BEM-VINDO AO LASTFM ================='),
    writeln(''),
    writeln('1 - Cadastrar Novo Usuário'),
    writeln('2 - Fazer Login'),
    writeln('3 - Ver Estatísticas Globais'),
    writeln('4 - Ver Ranking Global'),
    writeln('5 - Sair'),
    write('\nEscolha uma opção: '), flush_output,
    read_line_to_string(user_input, Opcao),
    executar_opcao_menu(Opcao, Usuarios).

executar_opcao_menu("1", Usuarios) :- handle_cadastro(Usuarios).
executar_opcao_menu("2", Usuarios) :- handle_login(Usuarios).
executar_opcao_menu("3", Usuarios) :- estatisticas_globais, menu(Usuarios).
executar_opcao_menu("4", Usuarios) :- gerar_ranking_global, menu(Usuarios).
executar_opcao_menu("5", _Usuarios) :- writeln('Encerrando o programa. Até logo!'), halt.
executar_opcao_menu(_, Usuarios) :- writeln('\nOpção inválida! Tente novamente.'), menu(Usuarios).

handle_cadastro(Usuarios) :-
    writeln('\nDigite seu Nome: '), read_line_to_string(user_input, Nome),
    writeln('Digite seu Email: '), read_line_to_string(user_input, Email),
    writeln('Digite sua Senha: '), read_line_to_string(user_input, Senha),
    (   \+ valida_nome(Nome) ->
        writeln('\nNome inválido! Use apenas letras e espaços.'),
        menu(Usuarios)
    ;   \+ valida_email(Email) ->
        writeln('\nEmail inválido! Insira um email válido.'),
        menu(Usuarios)
    ;   member(U, Usuarios), U.email == Email ->
        writeln('\nJá existe um usuário cadastrado com esse email!'),
        menu(Usuarios)
    ;   NovoUsuario = _{nome:Nome, email:Email, senha:Senha},
        cadastrar_usuario(NovoUsuario, Usuarios, UsuariosAtualizados),
        writeln('\nUsuário cadastrado com sucesso!'),
        menu(UsuariosAtualizados)
    ).

handle_login(Usuarios) :-
    writeln('\nDigite seu Email: '), read_line_to_string(user_input, Email),
    writeln('Digite sua Senha: '), read_line_to_string(user_input, Senha),
    login_usuario(Email, Senha, Usuarios, Resultado),
    (   Resultado = just(Usuario) ->
        format('\nBem-vindo(a) de volta, ~s!~n', [Usuario.nome]),
        menu_logado(Usuario)
    ;   writeln('\nEmail ou senha incorretos. Tente novamente.'),
        menu(Usuarios)
    ).

menu_logado(Usuario) :-
    writeln('\n================= MENU USUÁRIO LOGADO ================='),
    format('Olá, ~s!~n', [Usuario.nome]),
    writeln(''),
    writeln('1 - Ver Perfil e Histórico'),
    writeln('2 - Ver Minhas Conquistas'),
    writeln('3 - Registrar Scrobble'),
    writeln('4 - Ver Ranking Pessoal'),
    writeln('5 - Receber Recomendações'),
    writeln('6 - Calcular Compatibilidade'),
    writeln('7 - Logout'),
    write('\nEscolha uma opção: '), flush_output,
    read_line_to_string(user_input, Opcao),
    executar_opcao_logado(Opcao, Usuario).

executar_opcao_logado("1", Usuario) :- mostrar_perfil(Usuario), menu_logado(Usuario).
executar_opcao_logado("2", Usuario) :- ver_conquistas(Usuario), menu_logado(Usuario).
executar_opcao_logado("3", Usuario) :- handle_scrobble(Usuario).
executar_opcao_logado("4", Usuario) :- gerar_ranking_pessoal(Usuario), menu_logado(Usuario).
executar_opcao_logado("5", Usuario) :- handle_recomendacao(Usuario).
executar_opcao_logado("6", Usuario) :- handle_compatibilidade(Usuario).
executar_opcao_logado("7", _Usuario) :- writeln("\nFazendo logout..."), main.
executar_opcao_logado(_, Usuario) :- writeln('Opção inválida! Tente novamente.'), menu_logado(Usuario).

handle_scrobble(Usuario) :-
    carregar_catalogo(Catalogo),
    (   Catalogo = [] ->
        writeln('Nenhuma música disponível no catálogo.'),
        menu_logado(Usuario)
    ;   writeln('\nEscolha uma música para scrobble:'),
        listar_musicas_enumeradas(Catalogo, 1),
        write('\nDigite o número da música: '), flush_output,
        read_line_to_string(user_input, Entrada),
        (   catch(atom_number(Entrada, N), _, fail),
            length(Catalogo, Len), integer(N), N > 0, N =< Len ->
            nth1(N, Catalogo, MusicaEscolhida),
            registrar_scrobble(Usuario, MusicaEscolhida, UsuarioAtualizado),
            menu_logado(UsuarioAtualizado)
        ;   writeln('Entrada inválida. Tente novamente.'),
            menu_logado(Usuario)
        )
    ).

handle_recomendacao(Usuario) :-
    writeln('\nEscolha o tipo de recomendação:'),
    writeln('1 - Por gênero'), writeln('2 - Por artista'), writeln('3 - Baseada no histórico'),
    write('Escolha uma opção: '), flush_output,
    read_line_to_string(user_input, TipoStr),
    (   member(TipoStr, ["1","2","3"]) ->
        (   TipoStr == "1" ->
            listar_generos_disponiveis,
            write('\nDigite o nome do gênero: '), flush_output,
            read_line_to_string(user_input, Parametro)
        ;   TipoStr == "2" ->
            write('\nDigite o nome do artista: '), flush_output,
            read_line_to_string(user_input, Parametro)
        ;   Parametro = ""
        ),
        recomendar_musicas(Usuario, TipoStr, Parametro, Musicas),
        (   Musicas = [] ->
            writeln('\nNenhuma recomendação encontrada.')
        ;   writeln('\n===== Essas soam que nem você ====='),
            forall(member(M, Musicas), (
                genero_string_atom(GStr, M.genero),
                format('- ~s - ~s (~s)~n', [M.titulo, M.artista, GStr])
            ))
        )
    ;   writeln('Opção inválida!')
    ),
    menu_logado(Usuario).

handle_compatibilidade(Usuario) :-
    write('\nDigite o email do usuário para calcular o match: '), flush_output,
    read_line_to_string(user_input, EmailOutro),
    (   EmailOutro == Usuario.email ->
        writeln('\nVocê não pode calcular compatibilidade com você mesmo.'),
        menu_logado(Usuario)
    ;   carregar_usuarios(TodosUsuarios),
        (   member(OutroUsuario, TodosUsuarios), OutroUsuario.email == EmailOutro ->
            verificar_compatibilidade(Usuario, OutroUsuario, CompatibilidadeFloat),
            Compatibilidade is round(CompatibilidadeFloat * 100),
            format('\nSeu match com ~s é de: >>> ~d%% <<<\n', [OutroUsuario.nome, Compatibilidade]),
            (   Compatibilidade >= 80 -> writeln('Que match hein!?')
            ;   Compatibilidade =< 50 -> writeln('Xiii... talvez vocês devam descobrir algo em comum.')
            ;   writeln('Nada mau! Vejo bons interesses em comum!')
            )
        ;   writeln('\nUsuário não encontrado!')
        ),
        menu_logado(Usuario)
    ).

listar_musicas_enumeradas([], _).
listar_musicas_enumeradas([Musica | Resto], Index) :-
    format('~d - ~s - ~s~n', [Index, Musica.titulo, Musica.artista]),
    NextIndex is Index + 1,
    listar_musicas_enumeradas(Resto, NextIndex).

listar_generos_disponiveis :-
    writeln('\n--- GÊNEROS DISPONÍVEIS ---'),
    findall(GStr, genero_string_atom(GStr, _), Generos),
    forall(member(G, Generos), format('- ~s~n', [G])).

