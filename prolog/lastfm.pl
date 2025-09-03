:- use_module(library(readutil)).
:- use_module(library(date)).
:- use_module(catalogo).
:- use_module(library(lists)).

usuarios_arquivo('usuarios.json').
scrobbles_arquivo('scrobbles.json').

% --- SALVAR E CARREGAR DADOS ---

salvar_usuarios(Usuarios) :-
    usuarios_arquivo(Arquivo),
    open(Arquivo, write, Stream),
    forall(member(U, Usuarios),
           format(Stream, 'usuario(~q, ~q, ~q, ~q).~n', [U.nome, U.email, U.senha, U.conquistas])),
    close(Stream).

carregar_usuarios(Usuarios) :-
    usuarios_arquivo(Arquivo),
    ( exists_file(Arquivo),
      size_file(Arquivo, Size),
      Size > 0 ->
        consult(Arquivo),
        findall(_{nome:Nome, email:Email, senha:Senha, conquistas:Conquistas},
                usuario(Nome, Email, Senha, Conquistas), Usuarios)
    ; Usuarios = []
    ).

carregar_musicas(Musicas) :-
    catalogo:catalogo_musicas(Musicas).

salvar_scrobbles(Scrobbles) :-
    scrobbles_arquivo(Arquivo),
    open(Arquivo, write, Stream),
    forall(member(S, Scrobbles),
           format(Stream, 'scrobble(~q, ~q, ~q).~n', [S.emailUsuario, S.musica, S.momento])),
    close(Stream).

carregar_scrobbles(Scrobbles) :-
    scrobbles_arquivo(Arquivo),
    ( exists_file(Arquivo) ->
        size_file(Arquivo, Size),
        ( Size > 0 ->
            consult(Arquivo),
            findall(_{emailUsuario:Email, musica:Musica, momento:Momento},
                    scrobble(Email, Musica, Momento), Scrobbles)
        ; Scrobbles = []
        )
    ; Scrobbles = []
    ).

% --- USUÁRIOS ---

existe_email(Email, Usuarios) :-
    member(U, Usuarios),
    U.email == Email.

cadastrar_usuario(Nome, Email, Senha) :-
    carregar_usuarios(Usuarios),
    ( existe_email(Email, Usuarios) ->
        writeln('Já existe um usuário cadastrado com esse email!')
    ; valida_nome(Nome),
      valida_email(Email) ->
        NovoUsuario = _{nome:Nome, email:Email, senha:Senha, conquistas:[]},
        append(Usuarios, [NovoUsuario], UsuariosAtualizados),
        salvar_usuarios(UsuariosAtualizados),
        writeln('Usuário cadastrado com sucesso!')
    ; writeln('Nome ou email inválidos. Tente novamente')
    ).

login(Email, Senha, Usuario) :-
    carregar_usuarios(Usuarios),
    member(Usuario, Usuarios),
    Usuario.email == Email,
    Usuario.senha == Senha.

% --- SCROBBLES ---

registrar_scrobble(Email, Musica) :-
    carregar_scrobbles(Scrobbles),
    get_time(Timestamp),
    stamp_date_time(Timestamp, DateTime, local),
    format_time(string(Momento), '%Y-%m-%d %H:%M:%S', DateTime),
    NovoScrobble = _{musica:Musica, emailUsuario:Email, momento:Momento},
    append(Scrobbles, [NovoScrobble], NovosScrobbles),
    salvar_scrobbles(NovosScrobbles),
    writeln('Scrobble registrado com sucesso!').

registrar_scrobble_por_numero(Email, NumeroMusicaStr) :-
    carregar_musicas(Musicas),
    length(Musicas, Total),
    catch(number_string(NumeroMusica, NumeroMusicaStr), _, fail),
    ( NumeroMusica >= 1, NumeroMusica =< Total ->
        nth1(NumeroMusica, Musicas, Musica),
        registrar_scrobble(Email, Musica)
    ; writeln('Opção inválida. Tente novamente.')
    ).

% --- VALIDAÇÕES ---

valida_nome(Nome) :-
    string_codes(Nome, Codes),
    exclude(=(32), Codes, CodesSemEspaco), % Ignorar espaços
    forall(member(C, CodesSemEspaco), code_type(C, alpha)).

valida_email(Email) :-
    sub_string(Email, _, 1, _, "@"),
    sub_string(Email, _, 1, _, "."),
    \+ sub_string(Email, _, 1, _, " ").

% --- MENU PRINCIPAL ---

menu :-
    writeln('================= BEM-VINDO AO LASTFM ================='),
    writeln('1 - Cadastrar Novo Usuário'),
    writeln('2 - Fazer Login'),
    writeln('3 - Ver Estatísticas Globais'),
    writeln('4 - Ver Ranking Global'),        
    writeln('5 - Sair'),
    read_line_to_string(user_input, Opcao),
    menu_opcao(Opcao).

menu_opcao("1") :-
    writeln('Digite seu Nome:'),
    read_line_to_string(user_input, Nome),
    writeln('Digite seu Email:'),
    read_line_to_string(user_input, Email),
    writeln('Digite sua Senha:'),
    read_line_to_string(user_input, Senha),
    cadastrar_usuario(Nome, Email, Senha),
    menu.

menu_opcao("2") :-
    writeln('Digite seu Email:'),
    read_line_to_string(user_input, Email),
    writeln('Digite sua Senha:'),
    read_line_to_string(user_input, Senha),
    ( login(Email, Senha, Usuario) ->
        format('Bem-vindo(a) de volta, ~w!~n', [Usuario.nome]),
        menu_usuario_logado(Usuario)
    ; writeln('Email ou senha incorretos. Tente novamente'),
      menu
    ).

menu_opcao("3") :-
    estatisticas_globais,
    menu.

menu_opcao("4") :-
    writeln('Funcionalidade de Ranking Global ainda não implementada.'),
    menu.

menu_opcao("5") :-
    writeln('Encerrando...').

menu_opcao(_) :-
    writeln('Opção inválida.'),
    menu.

% --- MENU USUÁRIO LOGADO ---

menu_usuario_logado(Usuario) :-
    nl,
    writeln('================= MENU USUÁRIO LOGADO ================='),
    format('Olá, ~w!~n', [Usuario.nome]),
    writeln(''),
    writeln('1 - Ver Perfil'),
    writeln('2 - Ver Conquistas'),
    writeln('3 - Registrar scrobble'),
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
    writeln('=========== SEU PERFIL ============'),
    format('Nome: ~w~n', [Usuario.nome]),
    format('Email: ~w~n', [Usuario.email]),
    carregar_scrobbles(Scrobbles),
    ( Scrobbles == [] ->
        writeln('Você ainda não tem scrobbles registrados.')
    ; include({Usuario}/[S]>>(get_dict(emailUsuario, S, EmailS), EmailS == Usuario.email), Scrobbles, ScrobblesUsuario),
      ( ScrobblesUsuario == [] ->
          writeln('Você ainda não tem scrobbles registrados.')
      ; writeln('Histórico de scrobbles do usuário:')
      )
    ),
    menu_usuario_logado(Usuario).


listar_scrobbles_usuario([]).
listar_scrobbles_usuario([S|Ss]) :-
    format('~w - ~w - ~w~n', [S.momento, S.musica.titulo, S.musica.artista]),
    listar_scrobbles_usuario(Ss).

menu_usuario_opcao("2", Usuario) :-
    nl,
    writeln('========= SUAS CONQUISTAS ========='),
    writeln('Funcionalidade ainda não implementada.'), % Placeholder
    menu_usuario_logado(Usuario).

menu_usuario_opcao("3", Usuario) :-
    carregar_musicas(Musicas),
    ( Musicas == [] ->
        writeln('Nenhuma música disponível no catálogo.'),
        menu_usuario_logado(Usuario)
    ; writeln('\nEscolha uma música para scrobble:'),
      listar_musicas(Musicas, 1),
      write('\nDigite o número da música: '),
      flush_output(current_output),
      read_line_to_string(user_input, Entrada),
      ( catch(number_string(N, Entrada), _, fail),
        N > 0, N =< length(Musicas) ->
          nth1(N, Musicas, MusicaEscolhida),
          registrar_scrobble(Usuario.email, MusicaEscolhida),
          menu_usuario_logado(Usuario)
      ; writeln('Entrada inválida. Tente novamente.'),
        menu_usuario_logado(Usuario)
      )
    ).

menu_usuario_opcao("4", Usuario) :-
    writeln('Funcionalidade de Ranking Pessoal ainda não implementada.'),
    menu_usuario_logado(Usuario).

menu_usuario_opcao("5", Usuario) :-
    writeln('Funcionalidade de Recomendação ainda não implementada.'),
    menu_usuario_logado(Usuario).

menu_usuario_opcao("6", Usuario) :-
    writeln('Funcionalidade de Compatibilidade ainda não implementada.'),
    menu_usuario_logado(Usuario).

menu_usuario_opcao("7", _) :-
    writeln('Fazendo Logout...'),
    menu.

menu_usuario_opcao(_, Usuario) :-
    writeln('Opção inválida. Tente novamente.'),
    menu_usuario_logado(Usuario).

% --- FUNÇÕES AUXILIARES ---

listar_musicas([], _).
listar_musicas([M|Ms], N) :-
    format('~d - ~w - ~w - ~w~n', [N, M.titulo, M.artista, M.album]),
    N1 is N + 1,
    listar_musicas(Ms, N1).

% --- ESTATÍSTICAS GLOBAIS ---

estatisticas_globais :-
    carregar_usuarios(Usuarios),
    carregar_scrobbles(Scrobbles),

    ( Scrobbles == [] ->
        writeln('\nNenhum dado disponível para estatísticas globais ainda.')
    ; 
        % Lista de artistas
        findall(Artista, (member(S, Scrobbles), get_artista(S, Artista)), ListaArtistas),

        max_por_frequencia(ListaArtistas, ArtistaTop, CountArt),

        findall((Titulo, Artista), (member(S, Scrobbles), get_musica(S, Titulo, Artista)), ListaMusicas),

        max_por_frequencia(ListaMusicas, (TituloTop, ArtistaTopMus), CountMus),

        % Soma duracao
        findall(Duracao, (member(S, Scrobbles), get_duracao(S, Duracao)), Duracoes),
        sum_list(Duracoes, TempoTotal),

        % Usuarios com scrobbles
        include({Scrobbles}/[Usuario]>>tem_scrobble(Scrobbles, Usuario), Usuarios, UsuariosComScrobble),
        length(UsuariosComScrobble, QtdUsuarios),

        ( QtdUsuarios > 0 ->
            TempoMedio is TempoTotal // QtdUsuarios
        ; TempoMedio = 0
        ),

        format('\nEstatísticas Globais da Plataforma:\n'),
        format('  Artista mais ouvido: ~w (~d scrobbles)\n', [ArtistaTop, CountArt]),
        format('  Música mais ouvida: ~w - ~w (~d scrobbles)\n', [TituloTop, ArtistaTopMus, CountMus]),
        format('  Tempo médio de escuta por usuário: ~w\n', [format_tempo(TempoMedio)])
    ).

get_artista(Scrobble, Artista) :-
    Artista = Scrobble.musica.artista.

get_musica(Scrobble, Titulo, Artista) :-
    Titulo = Scrobble.musica.titulo,
    Artista = Scrobble.musica.artista.

get_duracao(Scrobble, Duracao) :-
    Duracao = Scrobble.musica.duracao.

tem_scrobble(Scrobbles, Usuario) :-
    member(Scrobble, Scrobbles),
    Scrobble.emailUsuario == Usuario.email,
    !.

max_por_frequencia(Lista, ElementoMax, CountMax) :-
    freq_lista(Lista, Frequencias),
    keysort(Frequencias, FrequenciasOrdenadas),
    reverse(FrequenciasOrdenadas, [CountMax-ElementoMax | _]).

freq_lista(Lista, Frequencias) :-
    sort(Lista, ElementosUnicos),
    findall(Count-Elemento,
        ( member(Elemento, ElementosUnicos),
          include(=(Elemento), Lista, SubLista),
          length(SubLista, Count)
        ),
        Frequencias).

format_tempo(Segundos) :-
    Hours is Segundos // 3600,
    Minutos is (Segundos mod 3600) // 60,
    Segs is Segundos mod 60,
    format('~02d:~02d:~02d', [Hours, Minutos, Segs]).

% --- PONTO DE ENTRADA ---

:- initialization(menu).
