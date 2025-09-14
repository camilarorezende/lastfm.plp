:- use_module('../Funcionalidades/Funcionalidades').
:- use_module(library(strings)).
:- initialization(run).
:- encoding(utf8).


inicializar :-
    carregar_usuarios_json,
    carregar_musicas(_),
    carregar_scrobbles(_).

% Conversão segura só para impressão
valor_para_string_safe(V, S) :-
    ( string(V) -> S = V
    ; atom(V)   -> atom_string(V, S)
    ; number(V) -> number_string(V, S)
    ; S = ""
    ).

% --------------------------- MENU PRINCIPAL ---------------------------

menu :-
    carregar_scrobbles(_),
    writeln('================= BEM-VINDO AO LASTFM ================='),
    writeln('1 - Cadastrar Novo Usuário'),
    writeln('2 - Fazer Login'),
    writeln('3 - Ver Estatísticas Globais'),
    writeln('4 - Ver Ranking Global'),
    writeln('5 - Sair'),
    read_line_to_string(user_input, Opcao),
    menu_opcao(Opcao).

menu_opcao("1") :-
    writeln('Digite seu Nome:'),  read_line_to_string(user_input, Nome),
    writeln('\nDigite seu Email:'), read_line_to_string(user_input, Email),
    writeln('\nDigite sua Senha:'), read_line_to_string(user_input, Senha),
    cadastrar_usuario(Nome, Email, Senha),
    menu.
menu_opcao("2") :-
    writeln('\nDigite seu Email:'), read_line_to_string(user_input, Email),
    writeln('\nDigite sua Senha:'), read_line_to_string(user_input, Senha),
    (   login(Email, Senha, Usuario)
    ->  format('\nBem-vindo(a) de volta, ~w!~n', [Usuario.nome]),
        menu_usuario_logado(Usuario)
    ;   writeln('Email ou senha incorretos. Tente novamente.'),
        menu
    ).
menu_opcao("3") :- estatisticas_globais, menu.
menu_opcao("4") :- gerar_ranking_global, menu.
menu_opcao("5") :- writeln('Encerrando...').
menu_opcao(_)  :- writeln('Opção inválida.'), menu.

% --------------------- MENU USUÁRIO LOGADO ----------------------------

menu_usuario_logado(Usuario) :-
    nl,
    writeln('================= MENU USUÁRIO LOGADO ================='),
    format('Olá, ~w!~n', [Usuario.nome]),
    writeln(''),
    writeln('1 - Ver Perfil'),
    writeln('2 - Ver Conquistas'),
    writeln('3 - Registrar Scrobble'),
    writeln('4 - Ver Ranking Pessoal'),
    writeln('5 - Receber Recomendação'),
    writeln('6 - Calcular Compatibilidade'),
    writeln('7 - Voltar ao Menu Principal'),
    write('\nEscolha uma opção: '),
    flush_output(current_output),
    read_line_to_string(user_input, Opcao),
    menu_usuario_opcao(Opcao, Usuario).

menu_usuario_opcao("1", Usuario) :-
    nl,
    writeln('=========== SEU PERFIL ============'), nl,
    format('Nome: ~w~n', [Usuario.nome]),
    format('Email: ~w~n', [Usuario.email]),
    carregar_scrobbles(Scs),
    include({Usuario}/[S]>>(
        is_dict(S),
        get_dict(emailUsuario, S, E),
        string_lower(E, E1),
        string_lower(Usuario.email, E2),
        E1 == E2
    ), Scs, ScsU),
    ( ScsU == [] ->
        writeln('\nVocê ainda não tem scrobbles registrados.')
    ;   writeln('\nHistórico de scrobbles do usuário:'),
        listar_scrobbles_usuario(ScsU)
    ),
    nl, menu_usuario_logado(Usuario).

menu_usuario_opcao("2", Usuario) :-
    nl, writeln('========= SUAS CONQUISTAS ========='),
    ver_conquistas(Usuario.email),
    nl, menu_usuario_logado(Usuario).

menu_usuario_opcao("3", Usuario) :-
    carregar_musicas(Ms),
    ( Ms == [] ->
        writeln('\nNenhuma música disponível no catálogo.'),
        menu_usuario_logado(Usuario)
    ;   writeln('\nEscolha uma música para scrobble:'),
        listar_musicas(Ms, 1),
        write('\nDigite o número da música: '), flush_output(current_output),
        read_line_to_string(user_input, Entrada),
        ( catch(number_string(N, Entrada), _, fail),
          nth1_safe(N, Ms, MEscolhida) ->
            registrar_scrobble(Usuario.email, MEscolhida),
            writeln('\nScrobble registrado com sucesso!'),
            menu_usuario_logado(Usuario)
        ;   writeln('\nEntrada inválida. Tente novamente.'),
            menu_usuario_logado(Usuario)
        )
    ).

menu_usuario_opcao("4", Usuario) :-
    gerar_ranking_pessoal(Usuario.email),
    menu_usuario_logado(Usuario).

menu_usuario_opcao("5", Usuario) :-
    writeln('\nEscolha o tipo de recomendação:'),
    writeln('\n1 - Por gênero'),
    writeln('2 - Por artista'),
    writeln('3 - Baseada no histórico'),
    write('\nEscolha uma opção: '), flush_output,
    read_line_to_string(user_input, RawTipoStr),
    split_string(RawTipoStr, "\n\r\t ", "\n\r\t ", Parts),
    atomics_to_string(Parts, "", TipoStr),
    (   member(TipoStr, ["1","2","3"]) ->
        ( TipoStr == "1" ->
            escolher_genero(Parametro)
        ; TipoStr == "2" ->
            write('\nDigite o nome do artista: '), flush_output,
            read_line_to_string(user_input, RawParam),
            normalizar_string(RawParam, Parametro)
        ; Parametro = ""
        ),
        recomendar_musicas(Usuario, TipoStr, Parametro, Musicas),
        ( Musicas == [] ->
            writeln('\nNenhuma recomendação encontrada.')
        ;   writeln('\n===== Essas soam que nem você ====='),
            forall(member(M, Musicas), (
                valor_para_string_safe(M.titulo,  TT),
                valor_para_string_safe(M.artista, TA),
                valor_para_string_safe(M.genero,  TG),
                format('- ~s - ~s (~s)~n', [TT, TA, TG])
            ))
        )
    ;   writeln('\nOpção inválida! Tente novamente.')
    ),
    menu_usuario_logado(Usuario).


menu_usuario_opcao("6", Usuario) :-
    writeln('\nDigite o email do outro usuário para comparar:'),
    read_line_to_string(user_input, Email2),
    carregar_usuarios(Usuarios),
    ( member(UTerm, Usuarios), UTerm = usuario(_, Email2, _, _) ->
        usuario_para_dict(UTerm, U2),
        verificar_compatibilidade(Usuario, U2)
    ; writeln('\nUsuário não encontrado. Tente novamente.')
    ),
    menu_usuario_logado(Usuario).

menu_usuario_opcao("7", _) :-
    writeln('\nFazendo logout...'),
    menu.

menu_usuario_opcao(_, Usuario) :-
    writeln('\nOpção inválida. Tente novamente.'),
    menu_usuario_logado(Usuario).

normalizar_string(In, Out) :-
    split_string(In, " ", " \t\n\r", Parts),
    exclude(==( ""), Parts, CleanParts),
    atomics_to_string(CleanParts, " ", Out).

nth1_safe(N, List, Elem) :-
    integer(N), N > 0, length(List, Len), N =< Len, nth1(N, List, Elem).

run :-
    inicializar,
    menu.