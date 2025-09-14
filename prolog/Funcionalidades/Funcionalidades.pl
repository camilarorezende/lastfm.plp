:- dynamic usuario/4.
:- use_module(library(http/json)).
:- use_module(library(date)).
:- consult('Conquistas.pl').
:- discontiguous scrobble_do_usuario/2.
:- discontiguous compare_scrobbles/3.



% --------------------- Caminhos dos arquivos JSON ---------------------

caminho_usuarios('../usuarios.json').
caminho_musicas('../catalogo.json').
caminho_scrobbles('../scrobbles.json').

% --------------------- Validações ---------------------

valid_usuario(Nome) :-
    string(Nome),
    re_match("^[a-zA-Z ]+$", Nome).

valid_email(Email) :-
    string(Email),
    re_match("^[\\w._%+-]+@[\\w.-]+\\.[a-zA-Z]{2,}$", Email).

% --------------------- Usuários ---------------------


salvar_usuarios_json :-
    caminho_usuarios(File),
    findall(
        json([nome=Nome, email=Email, senha=Senha, conquistas=Conqs]),
        usuario(Nome, Email, Senha, Conqs),
        UsuariosJSON
    ),
    open(File, write, Stream),
    json_write_dict(Stream, UsuariosJSON),
    close(Stream).

carregar_usuarios_json :-
    caminho_usuarios(File),
    ( exists_file(File) ->
        open(File, read, Stream),
        json_read_dict(Stream, UsuariosJSON),
        close(Stream),
        retractall(usuario(_, _, _, _)),
        carregar_lista_usuarios(UsuariosJSON)
    ; true
    ).

carregar_lista_usuarios([]).
carregar_lista_usuarios([Elem | T]) :-
    % Elem pode ser dict ou json(...) — vamos normalizar
    (   Elem = json(AssocList) -> dict_create(Dict, _, AssocList)
    ;   Elem = Dict  % já é dict
    ),
    % extraímos campos
    Nome = Dict.nome,
    Email = Dict.email,
    Senha = Dict.senha,
    ( var(Dict.conquistas) ; Dict.conquistas == null -> Conqs = [] ; Conqs = Dict.conquistas ),
    assertz(usuario(Nome, Email, Senha, Conqs)),
    carregar_lista_usuarios(T).

carregar_usuarios(Usuarios) :-
    carregar_usuarios_json,
    findall(usuario(Nome, Email, Senha, Conqs), usuario(Nome, Email, Senha, Conqs), Usuarios).

cadastrar_usuario(Nome, Email, Senha) :-
    valid_usuario(Nome),
    valid_email(Email),
    \+ usuario(_, Email, _, _),
    assertz(usuario(Nome, Email, Senha, [])),
    salvar_usuarios_json,
    writeln('Usuário cadastrado com sucesso!').

cadastrar_usuario(Nome, Email, _) :-
    ( \+ valid_usuario(Nome) ->
        writeln('Nome inválido: deve conter apenas letras e espaços')
    ; \+ valid_email(Email) ->
        writeln('Email inválido. Tente novamente.')
    ; usuario(_, Email, _, _) ->
        writeln('Já existe um usuário com esse email!')
    ).

login(EmailInput, SenhaInput, usuario{nome:Nome, email:Email}) :-
    usuario(Nome, Email, Senha, _),
    string_lower(Email, EmailLower),
    string_lower(EmailInput, EmailInputLower),
    EmailLower == EmailInputLower,
    SenhaInput == Senha.



% --------------------- Músicas ---------------------

carregar_musicas(MusicasDict) :-
    caminho_musicas(File),
    ( exists_file(File) ->
        open(File, read, Stream),
        json_read(Stream, MusicasJSON),
        close(Stream),
        converter_json_para_dicts(MusicasJSON, MusicasDict)
    ; MusicasDict = []
    ).

converter_json_para_dicts([], []).
converter_json_para_dicts([json(Obj)|T], [Dict|DT]) :-
    dict_create(Dict, _, Obj),
    converter_json_para_dicts(T, DT).

listar_musicas([], _).
listar_musicas([M|Ms], N) :-
    format('~d - ~w - ~w - ~w~n', [N, M.titulo, M.artista, M.album]),
    N1 is N + 1,
    listar_musicas(Ms, N1).

% --------------------- Scrobbles ---------------------

carregar_scrobbles(Scrobbles) :-
    caminho_scrobbles(File),
    ( exists_file(File) ->
        catch(
            ( open(File, read, Stream),
              json_read(Stream, JSON),
              close(Stream),
              converter_scrobbles(JSON, Scrobbles)
            ),
            _Erro,
            Scrobbles = []
        )
    ; Scrobbles = []
    ).


converter_scrobbles([], []).
converter_scrobbles([json(Obj)|T], [NovoDict|ST]) :-
    dict_create(Dict, _, Obj),
    ( get_dict(musica, Dict, MusicaData) ->
        ( MusicaData = json(MusicaAssoc) ->
            dict_create(MusicaDict, _, MusicaAssoc)
        ; MusicaData = MusicaDict  % já é um dict
        ),
        put_dict(musica, Dict, MusicaDict, NovoDict)
    ; NovoDict = Dict
    ),
    converter_scrobbles(T, ST).



listar_scrobbles_usuario(Scrobbles) :-
    listar_scrobbles_usuario(Scrobbles, 1).

listar_scrobbles_usuario([], _).
listar_scrobbles_usuario([S|Ss], N) :-
    format('  ~d. ~w - ~w | ~w~n', [N, S.musica.titulo, S.musica.artista, S.momento]),
    N1 is N + 1,
    listar_scrobbles_usuario(Ss, N1).


registrar_scrobble(EmailUsuario, MusicaDict) :-
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTime, local),
    format_time(atom(Momento), '%Y-%m-%d %H:%M:%S', DateTime),
    Scrobble = scrobble{
        emailUsuario: EmailUsuario,
        momento: Momento,
        musica: MusicaDict
    },
    salvar_scrobble(Scrobble),
    verificar_e_aplicar_conquistas(EmailUsuario).


salvar_scrobble(NovoScrobble) :-
    carregar_scrobbles(Scrobbles),
    append(Scrobbles, [NovoScrobble], Todos),
    salvar_scrobbles_json(Todos).

salvar_scrobbles_json(Scrobbles) :-
    caminho_scrobbles(File),
    maplist(scrobble_para_json, Scrobbles, JSONList),
    open(File, write, Stream),
    json_write_dict(Stream, JSONList),
    close(Stream).

scrobble_para_json(ScrobbleDict, json([
    emailUsuario=ScrobbleDict.emailUsuario,
    momento=ScrobbleDict.momento,
    musica=ScrobbleDict.musica
])).

% --------------------- Conquistas ---------------------

ver_conquistas(Email) :-
    usuario(_, Email, _, Conqs),
    ( Conqs = [] ->
        writeln('Você ainda não possui conquistas.')
    ;
        forall(member(C, Conqs), format('- ~w~n', [C]))
    ), !.
ver_conquistas(_) :-
    writeln('Usuário não encontrado.').

adicionar_conquista(Email, Conquista) :-
    usuario(Nome, Email, Senha, Conqs),
    ( member(Conquista, Conqs) ->
        writeln('Usuário já possui essa conquista.')
    ;
        retract(usuario(Nome, Email, Senha, Conqs)),
        assertz(usuario(Nome, Email, Senha, [Conquista|Conqs])),
        salvar_usuarios_json,
        writeln('Nova conquista desbloqueada!')
    ), !.
adicionar_conquista(_, _) :-
    writeln('Usuário não encontrado.').

verificar_e_aplicar_conquistas(EmailUsuario) :-
    carregar_scrobbles(Scrobbles),
    findall(Conquista,
        (conquista(Scrobbles, EmailUsuario, Conquista),
         \+ ja_tem_conquista(EmailUsuario, Conquista)
        ),
        Novas),
    forall(member(C, Novas), adicionar_conquista(EmailUsuario, C)).

ja_tem_conquista(Email, Conquista) :-
    usuario(_, Email, _, Conqs),
    member(Conquista, Conqs).


% --------------------- Estatísticas ---------------------

tempo_total_usuario(Scrobbles, usuario(_, Email, _, _), Total) :-
    include(
        {Email}/[S]>>(
            get_dict(emailUsuario, S, EmailS),
            atom_string(EmailS, Email)
        ),
        Scrobbles,
        ScrobblesUsuario
    ),
    findall(Dur,
        ( member(S, ScrobblesUsuario),
          get_dict(musica, S, Musica),
          ( get_dict(duracao, Musica, Dur) -> true ; Dur = 0 )
        ),
        Duracoes),
    sum_list(Duracoes, Total).



estatisticas_globais :-
    carregar_usuarios(Usuarios),
    carregar_scrobbles(Scrobbles),
    ( Scrobbles == [] ->
        writeln('\nNenhum dado disponível para estatísticas globais ainda.')
    ;
        findall(Artista, (member(S, Scrobbles), get_artista(S, Artista)), ListaArtistas),
        max_por_frequencia(ListaArtistas, ArtistaTop, CountArt),

        findall((Titulo, Artista), (member(S, Scrobbles), get_musica(S, Titulo, Artista)), ListaMusicas),
        max_por_frequencia(ListaMusicas, (TituloTop, ArtistaTopMus), CountMus),

        include(
            {Scrobbles}/[Usuario]>>tem_scrobble(Scrobbles, Usuario),
            Usuarios,
            UsuariosComScrobbles
        ),

        findall(
            TempoTotal,
            (member(U, UsuariosComScrobbles), tempo_total_usuario(Scrobbles, U, TempoTotal)),
            TemposTotaisUsuarios
        ),

        length(TemposTotaisUsuarios, QtdUsuarios),

         ( QtdUsuarios > 0 ->
           sum_list(TemposTotaisUsuarios, SomaTempos),
           TempoMedio is SomaTempos / QtdUsuarios
        ; TempoMedio = 0
        ),


        format_tempo(TempoMedio, TempoFormatado),

        format('\nEstatísticas Globais da Plataforma:\n'),
        format('  Artista mais ouvido: ~w (~d scrobble(s))\n', [ArtistaTop, CountArt]),
        format('  Música mais ouvida: ~w - ~w (~d scrobble(s))\n', [TituloTop, ArtistaTopMus, CountMus]),
        format('  Tempo médio de escuta por usuário: ~w\n', [TempoFormatado])
    ).

gerar_ranking_pessoal(UsuarioEmail) :-
    carregar_scrobbles(Scrobbles),
    
    % Filtra scrobbles do usuário corretamente:
    findall(MusicaDict,
            (member(Scrobble, Scrobbles),
             get_dict(emailUsuario, Scrobble, Email),
             string_lower(Email, EL),
             string_lower(UsuarioEmail, UL),
             EL == UL,
             get_dict(musica, Scrobble, MusicaDict)),
            MusicasOuvidasDicts),
    
    ( MusicasOuvidasDicts == [] ->
        writeln('Você ainda não tem nenhum scrobble :( Que tal dar play em alguma música? ;)')
    ; 
    
        maplist(dict_para_musica_termo, MusicasOuvidasDicts, MusicasOuvidas),
        
        contar_frequencias(MusicasOuvidas, ContagemMusicas),
        ordenar_por_valor_decrescente(ContagemMusicas, RankMusicas),

        findall(Genero, (member(musica(_, _, Genero), MusicasOuvidas)), Generos),
        contar_frequencias(Generos, ContagemGeneros),
        ordenar_por_valor_decrescente(ContagemGeneros, RankGeneros),

        writeln('\nRanking das suas músicas mais escutadas! Veja seus hits do momento:'),
        imprimir_rank_musicas(RankMusicas),

        writeln('\nRanking dos gêneros mais ouvidos:'),
        imprimir_rank_generos(RankGeneros)
    ).

dict_para_musica_termo(Dict, musica(TituloS, ArtistaS, GeneroS)) :-
    dict_field_to_string(Dict.titulo, TituloS),
    dict_field_to_string(Dict.artista, ArtistaS),
    dict_field_to_string(Dict.genero, GeneroS).

% Garante que o valor seja string
dict_field_to_string(Value, Str) :-
    (   string(Value) -> Str = Value
    ;   atom(Value)   -> atom_string(Value, Str)
    ;   number(Value) -> number_string(Value, Str)
    ;   Str = ""  % fallback
    ).


musica_genero(musica(_, _, Genero), Genero).


contar_frequencias(Lista, Contagem) :-
    setof((X, N), 
          aggregate(count, member(X, Lista), N), 
          Contagem).


incrementa_ou_adiciona(X, [], [(X,1)]) :-
    format("Adicionando nova música: ~w~n", [X]).

incrementa_ou_adiciona(X, [(Y,N)|T], [(Y,N1)|T]) :-
    X == Y,
    N1 is N + 1,
    format("Incrementando contagem de música: ~w (de ~d para ~d)~n", [X, N, N1]).

incrementa_ou_adiciona(X, [H|T], [H|NT]) :-
    H = (Y,_),
    X \== Y,
    incrementa_ou_adiciona(X, T, NT).

ordenar_por_valor_decrescente(Lista, Ordenada) :-
    predsort(compara_valor_decrescente, Lista, Ordenada).

compara_valor_decrescente(Delta, (X, V1), (Y, V2)) :-
    ( V1 > V2 -> Delta = '<'
    ; V1 < V2 -> Delta = '>'
    ; 
      % Se valores iguais, compara os nomes (se for musica, compara título; se for átomo/string, compara diretamente)
      compara_empate(X, Y, Delta)
    ).

compara_empate(musica(T1, _, _), musica(T2, _, _), Delta) :-
    compare(Delta, T1, T2).

compara_empate(X, Y, Delta) :-
    % Caso X e Y não sejam musica(...), compara diretamente
    compare(Delta, X, Y).



imprimir_rank_musicas([]).
imprimir_rank_musicas([(musica(Titulo, Artista, _), Qnt)|T]) :-
    format('~w - ~w | Ouvidas: ~d~n', [Titulo, Artista, Qnt]),
    imprimir_rank_musicas(T).

% imprimir ranking de gêneros
imprimir_rank_generos([]).
imprimir_rank_generos([(Genero, Qnt)|T]) :-
    format('~w | Ouvidas: ~d~n', [Genero, Qnt]),
    imprimir_rank_generos(T).


gerar_ranking_global :-
    carregar_usuarios(Usuarios),
    carregar_scrobbles(Scrobbles),
    (   Usuarios == [] ->
        writeln('\nNenhum usuário cadastrado ainda. Que tal ser o primeiro a se juntar ao LastFM? :)')
    ;   findall(Stats, (
            member(usuario(Nome, Email, _, _), Usuarios),
            include(scrobble_do_usuario(Email), Scrobbles, ScsU),
            length(ScsU, QtdScrobbles),
            Stats = stat{nome: Nome, email: Email, scrobbles: QtdScrobbles}
        ), ListaStats),
        predsort(compare_scrobbles, ListaStats, Ranking),
        writeln('\n========= RANKING GLOBAL =========='),
        imprime_rank_global(Ranking)
    ).

scrobble_do_usuario(Email, Scrobble) :-
    get_dict(emailUsuario, Scrobble, EmailScrobble),
    string_lower(Email, Lower1),
    string_lower(EmailScrobble, Lower2),
    Lower1 == Lower2.

compare_scrobbles(Order, A, B) :-
    A.scrobbles >= B.scrobbles -> Order = (<) ; Order = (>).

imprime_rank_global([], _).
imprime_rank_global([Stat | T], Pos) :-
    format('~d. ~w (~w) - ~d scrobble(s)~n', [Pos, Stat.nome, Stat.email, Stat.scrobbles]),
    Pos1 is Pos + 1,
    imprime_rank_global(T, Pos1).
imprime_rank_global(Ranking) :-
    imprime_rank_global(Ranking, 1).

verificar_compatibilidade(U1, U2) :-
    verificar_compatibilidade(U1, U2, Compat),
    Porcentagem is Compat * 100,
    format('Compatibilidade entre ~w e ~w: ~2f%%~n', [U1.email, U2.email, Porcentagem]).

verificar_compatibilidade(U1, U2, Compatibilidade) :-
    carregar_scrobbles(Scrobbles),
    include(scrobble_do_usuario(U1.email), Scrobbles, ScsU1),
    include(scrobble_do_usuario(U2.email), Scrobbles, ScsU2),
    findall(G, (member(S, ScsU1), G = S.musica.genero), Gs1), list_to_set(Gs1, SetG1),
    findall(A, (member(S, ScsU1), A = S.musica.artista), As1), list_to_set(As1, SetA1),
    findall(G, (member(S, ScsU2), G = S.musica.genero), Gs2), list_to_set(Gs2, SetG2),
    findall(A, (member(S, ScsU2), A = S.musica.artista), As2), list_to_set(As2, SetA2),
    intersection(SetG1, SetG2, ComunsG), length(ComunsG, LComunsG),
    intersection(SetA1, SetA2, ComunsA), length(ComunsA, LComunsA),
    ( length(SetG1, LSetG1), LSetG1 > 0 -> CompG is LComunsG / LSetG1 ; CompG = 0 ),
    ( length(SetA1, LSetA1), LSetA1 > 0 -> CompA is LComunsA / LSetA1 ; CompA = 0 ),
    Compatibilidade is (CompG * 0.6) + (CompA * 0.4).

% pega o gênero mais ouvido do usuário
genero_mais_ouvido(UsuarioEmail, GeneroMais) :-
    carregar_scrobbles(TodosScrobbles),
    include(scrobble_do_usuario(UsuarioEmail), TodosScrobbles, ScsUsuario),
    findall(Genero, (member(S, ScsUsuario), get_dict(genero, S.musica, Genero)), Generos),
    most_common(Generos, GeneroMais).


recomendar_musicas(Usuario, Opcao, Parametro, Recomendacoes) :-
    carregar_musicas(Musicas),
    carregar_scrobbles(TodosScrobbles),

    include(scrobble_do_usuario(Usuario.email), TodosScrobbles, HistoricoUsuario),

    findall(M, (member(S, HistoricoUsuario), M = S.musica), MusicasOuvidas),

    subtract(Musicas, MusicasOuvidas, NaoOuvidas),

    (   Opcao == '1' -> % Por gênero fornecido
        include(musica_tem_genero(Parametro), NaoOuvidas, Filtradas)
    ;   Opcao == '2' -> % Por artista fornecido
        include(musica_tem_artista(Parametro), NaoOuvidas, Filtradas)
    ;   Opcao == '3' -> % Automática baseada no gênero mais ouvido
        genero_mais_ouvido(Usuario.email, GeneroMais),
        (GeneroMais \= none ->
            include(musica_tem_genero(GeneroMais), NaoOuvidas, Filtradas)
        ;
            Filtradas = NaoOuvidas  
        )
    ;   Filtradas = []
    ),

    random_permutation(Filtradas, Embaralhadas),
    take(3, Embaralhadas, Recomendacoes).

genero_string_atom(String, Atom) :- atom_string(Atom, String).

musica_tem_genero(GeneroString, Musica) :-
    genero_string_atom(GeneroString, GeneroAtom),
    Musica.genero == GeneroAtom.

musica_tem_artista(Artista, Musica) :- Musica.artista == Artista.


escolher_genero(GeneroEscolhido) :-
    carregar_musicas(Musicas),
    findall(Genero, (member(M, Musicas), get_dict(genero, M, Genero)), Generos),
    sort(Generos, GenerosUnicos),

    writeln('Gêneros disponíveis:'),
    listar_generos_numerados(GenerosUnicos, 1),

    write('Escolha o número do gênero: '), flush_output,
    read_line_to_string(user_input, Entrada),
    ( catch(number_string(Num, Entrada), _, fail),
      nth1(Num, GenerosUnicos, GeneroEscolhido) ->
        true
    ; writeln('Opção inválida. tente novamente.'),
      escolher_genero(GeneroEscolhido)
    ).

listar_generos_numerados([], _).
listar_generos_numerados([G|Gs], N) :-
    format('~d - ~w~n', [N, G]),
    N1 is N + 1,
    listar_generos_numerados(Gs, N1).


get_artista(Scrobble, Artista) :- Artista = Scrobble.musica.artista.
get_musica(Scrobble, Titulo, Artista) :-
    Titulo = Scrobble.musica.titulo,
    Artista = Scrobble.musica.artista.
get_duracao(Scrobble, Duracao) :- Duracao = Scrobble.musica.duracao.

tem_scrobble(Scrobbles, usuario(_, Email, _, _)) :-
    atom_string(EmailAtom, Email),
    member(S, Scrobbles),
    get_dict(emailUsuario, S, EmailAtom).


max_por_frequencia(Lista, ElementoMax, CountMax) :-
    freq_lista(Lista, Frequencias),
    keysort(Frequencias, Ordenadas),
    reverse(Ordenadas, [CountMax-ElementoMax | _]).

freq_lista(Lista, Frequencias) :-
    sort(Lista, Unicos),
    findall(Count-Elem,
        ( member(Elem, Unicos),
          include(=(Elem), Lista, Sub),
          length(Sub, Count)
        ),
        Frequencias).

format_tempo(SegundosFloat, Formatado) :-
    Segundos is round(SegundosFloat),  % <- conversão segura para inteiro
    Horas is Segundos // 3600,
    Minutos is (Segundos mod 3600) // 60,
    SegRestantes is Segundos mod 60,
    format(atom(Formatado), '~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [Horas, Minutos, SegRestantes]).



conta_e_ordena(Lista, ContagemOrdenada) :-
    msort(Lista, ListaOrdenada),
    clumped(ListaOrdenada, Pares),
    maplist(item_contagem, Pares, Contagem),
    predsort(compare_contagem, Contagem, ContagemOrdenada).

compare_contagem(>, (_, C1), (_, C2)) :- C1 > C2.
compare_contagem(<, (_, C1), (_, C2)) :- C1 < C2.
compare_contagem(=, _, _).

compare_scrobbles(>, S1, S2) :- S1.scrobbles > S2.scrobbles.
compare_scrobbles(<, S1, S2) :- S1.scrobbles < S2.scrobbles.
compare_scrobbles(=, _, _).

item_contagem(Lista, (Item, Contagem)) :-
    Lista = [Item|_],
    length(Lista, Contagem).

take(N, _, Xs) :- N =< 0, !, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :-
    N1 is N - 1,
    take(N1, Xs, Ys).

usuario_para_dict(usuario(Nome, Email, Senha, Conqs), usuario{nome:Nome, email:Email, senha:Senha, conquistas:Conqs}).
