
:- module(funcionalidades, [
    carregar_dados_iniciais/1, 
    cadastrar_usuario/3,
    login_usuario/4,
    registrar_scrobble/3,
    historico_do_usuario/1,
    gerar_ranking_pessoal/1,
    gerar_ranking_global/0,
    ver_conquistas/1,
    recomendar_musicas/4,
    verificar_compatibilidade/3,
    estatisticas_globais/0
]).

:- use_module(library(http/json)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

:- use_module('../Types/genero').
:- use_module('../Types/musica').
:- use_module('../Types/scrobble').
:- use_module('../Types/usuario').
:- use_module('conquistas').

arquivo_usuarios('usuarios.json').
arquivo_scrobbles('scrobbles.json').
arquivo_catalogo('catalogo.json').
carregar_json(Arquivo, Dados) :-
    (   exists_file(Arquivo) ->
        setup_call_cleanup(
            open(Arquivo, read, Stream, [encoding(utf8)]),
            json_read_dict(Stream, Dados),
            close(Stream)
        )
    ;   Dados = []
    ).

salvar_json(Arquivo, Dados) :-
    setup_call_cleanup(
        open(Arquivo, write, Stream, [encoding(utf8)]),
        json_write_dict(Stream, Dados, [width(4)]),
        close(Stream)
    ).
carregar_dados_iniciais(UsuariosTipados) :-
    carregar_usuarios(UsuariosTipados).

carregar_usuarios(UsuariosTipados) :-
    arquivo_usuarios(Arquivo),
    carregar_json(Arquivo, UsuariosBrutos),
    maplist(json_to_usuario, UsuariosBrutos, UsuariosTipados).

salvar_usuarios(UsuariosTipados) :-
    arquivo_usuarios(Arquivo),
    maplist(usuario_to_json, UsuariosTipados, UsuariosBrutos),
    salvar_json(Arquivo, UsuariosBrutos).

carregar_scrobbles(ScrobblesTipados) :-
    arquivo_scrobbles(Arquivo),
    carregar_json(Arquivo, ScrobblesBrutos),
    maplist(json_to_scrobble, ScrobblesBrutos, ScrobblesTipados).

salvar_scrobbles(ScrobblesTipados) :-
    arquivo_scrobbles(Arquivo),
    maplist(scrobble_to_json, ScrobblesTipados, ScrobblesBrutos),
    salvar_json(Arquivo, ScrobblesBrutos).

carregar_catalogo(CatalogoTipado) :-
    arquivo_catalogo(Arquivo),
    carregar_json(Arquivo, CatalogoBruto),
    maplist(json_to_musica, CatalogoBruto, CatalogoTipado).

atualizar_usuario(UsuarioAtualizado) :-
    EmailAtualizado = UsuarioAtualizado.email,
    carregar_usuarios(UsuariosAntigos),
    maplist(atualizar_se_corresponder(EmailAtualizado, UsuarioAtualizado), UsuariosAntigos, UsuariosNovos),
    salvar_usuarios(UsuariosNovos).

atualizar_se_corresponder(EmailAlvo, UsuarioNovo, UsuarioAntigo, UsuarioFinal) :-
    (   UsuarioAntigo.email == EmailAlvo ->
        UsuarioFinal = UsuarioNovo
    ;   UsuarioFinal = UsuarioAntigo
    ).
valida_nome(Nome) :-
    string_length(Nome, L), L > 0,
    string_chars(Nome, Chars),
    forall(member(C, Chars), (char_type(C, alpha); char_type(C, space))).

valida_email(Email) :-
    string_length(Email, L), L > 0,
    sub_string(Email, _, _, _, "@"),
    sub_string(Email, _, _, _, "."),
    \+ sub_string(Email, _, _, _, " ").
cadastrar_usuario(NovoUsuario, Usuarios, UsuariosAtualizados) :-
    UsuariosAtualizados = [NovoUsuario | Usuarios],
    salvar_usuarios(UsuariosAtualizados).

login_usuario(Email, Senha, Usuarios, just(Usuario)) :-
    member(Usuario, Usuarios),
    Usuario.email == Email,
    Usuario.senha == Senha, !.
login_usuario(_, _, _, nothing).

registrar_scrobble(Usuario, Musica, UsuarioAtualizado) :-
    get_time(Timestamp),
    format_time(string(Momento), '%d/%m/%Y %H:%M', Timestamp),
    NovoScrobble = scrobble{musica:Musica, emailUsuario:Usuario.email, momento:Momento},
    carregar_scrobbles(Scrobbles),
    NovosScrobbles = [NovoScrobble | Scrobbles],
    salvar_scrobbles(NovosScrobbles),
    include(scrobble_do_usuario(Usuario.email), NovosScrobbles, ScrobblesDoUsuario),
    get_conquistas_usuario(ScrobblesDoUsuario, ConquistasPossiveis),
    subtract(ConquistasPossiveis, Usuario.conquistas, NovasConquistas),
    (   NovasConquistas \= [] ->
        writeln('\nParabéns! Você desbloqueou as seguintes conquistas:'),
        forall(member(C, NovasConquistas), format('- ~s~n', [C]))
    ;   true
    ),
    union(Usuario.conquistas, NovasConquistas, ConquistasAtualizadas),
    writeln('Scrobble registrado com sucesso!'),
    UsuarioAtualizado = Usuario.put(conquistas, ConquistasAtualizadas),
    atualizar_usuario(UsuarioAtualizado).

historico_do_usuario(Scrobbles) :-
    (   Scrobbles = [] ->
        writeln('\nVocê ainda não tem scrobbles registrados.')
    ;   writeln('\nSeu histórico de scrobbles:'),
        forall(member(S, Scrobbles), (
            M = S.musica,
            format('- ~s - ~s - ~s (~s)~n', [M.titulo, M.artista, M.album, S.momento])
        )),
        findall(D, (member(S, Scrobbles), D = S.musica.duracao), Duracoes),
        sum_list(Duracoes, TempoTotal),
        format_tempo(TempoTotal, TempoFormatado),
        format('\nTempo total escutado: ~s~n', [TempoFormatado])
    ).

gerar_ranking_pessoal(Usuario) :-
    carregar_scrobbles(TodosScrobbles),
    include(scrobble_do_usuario(Usuario.email), TodosScrobbles, ScrobblesUsuario),
    (   ScrobblesUsuario = [] ->
        writeln('Você ainda não tem nenhum scrobble :(')
    ;   findall(M, (member(S, ScrobblesUsuario), M = S.musica), MusicasOuvidas),
        conta_e_ordena(MusicasOuvidas, RankMusicas),
        writeln('\nRanking das suas músicas mais escutadas:'),
        printa_rank_musica(RankMusicas),
        findall(G, (member(M, MusicasOuvidas), G = M.genero), GenerosOuvidos),
        conta_e_ordena(GenerosOuvidos, RankGeneros),
        writeln('\nRanking dos gêneros mais ouvidos:'),
        printa_rank_genero(RankGeneros)
    ).

gerar_ranking_global :-
    carregar_usuarios(Usuarios),
    carregar_scrobbles(Scrobbles),
    (   Usuarios = [] ->
        writeln('\nNenhum usuário cadastrado ainda.')
    ;   findall(Stats, (
            member(U, Usuarios),
            include(scrobble_do_usuario(U.email), Scrobbles, ScsU),
            length(ScsU, QtdScrobbles),
            Stats = _{usuario:U, scrobbles:QtdScrobbles}
        ), ListaStats),
        predsort(compare_scrobbles, ListaStats, Ranking),
        writeln('\n===== RANKING GLOBAL ====='),
        imprime_rank_global(Ranking)
    ).

estatisticas_globais :-
    carregar_scrobbles(Scrobbles),
    (   Scrobbles = [] ->
        writeln('\nNenhum dado disponível para estatísticas.')
    ;   findall(Artista, (member(S, Scrobbles), Artista = S.musica.artista), Artistas),
        conta_e_ordena(Artistas, [(TopArtista, CntArtista)|_]),
        findall(Musica, (member(S, Scrobbles), Musica = S.musica.titulo), Musicas),
        conta_e_ordena(Musicas, [(TopMusica, CntMusica)|_]),
        writeln('\n===== ESTATÍSTICAS GLOBAIS ====='),
        format('  Artista mais ouvido: ~s (~d scrobbles)~n', [TopArtista, CntArtista]),
        format('  Música mais ouvida: ~s (~d scrobbles)~n', [TopMusica, CntMusica])
    ).

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

ver_conquistas(Usuario) :-
    (   get_dict(conquistas, Usuario, Conquistas), Conquistas \= [] ->
        writeln('\nConquistas desbloqueadas:'),
        forall(member(C, Conquistas), format('- ~s~n', [C]))
    ;   writeln('\nVocê ainda não desbloqueou nenhuma conquista.')
    ).

recomendar_musicas(Usuario, Opcao, Parametro, Recomendacoes) :-
    carregar_catalogo(Catalogo),
    carregar_scrobbles(TodosScrobbles),
    include(scrobble_do_usuario(Usuario.email), TodosScrobbles, HistoricoUsuario),
    findall(M, (member(S, HistoricoUsuario), M = S.musica), MusicasOuvidas),
    subtract(Catalogo, MusicasOuvidas, NaoOuvidas),
    (   Opcao == "1" -> % Por gênero
        include(musica_tem_genero(Parametro), NaoOuvidas, Filtradas)
    ;   Opcao == "2" -> % Por artista
        include(musica_tem_artista(Parametro), NaoOuvidas, Filtradas)
    ;   Opcao == "3" -> % Automática
        Filtradas = NaoOuvidas
    ;   Filtradas = []
    ),
    random_permutation(Filtradas, Embaralhadas),
    take(3, Embaralhadas, Recomendacoes).

scrobble_do_usuario(Email, Scrobble) :- Scrobble.emailUsuario == Email.

musica_tem_genero(GeneroString, Musica) :-
    genero_string_atom(GeneroString, GeneroAtom),
    Musica.genero == GeneroAtom.

musica_tem_artista(Artista, Musica) :- Musica.artista == Artista.

format_tempo(TotalSegundos, String) :-
    H is TotalSegundos // 3600,
    Resto1 is TotalSegundos mod 3600,
    M is Resto1 // 60,
    S is Resto1 mod 60,
    format(string(String), '~dh ~dm ~ds', [H, M, S]).

printa_rank_musica([]).
printa_rank_musica([(Musica, Qtd)|Resto]) :-
    format('~s - ~s | Ouvidas: ~d~n', [Musica.titulo, Musica.artista, Qtd]),
    printa_rank_musica(Resto).

printa_rank_genero([]).
printa_rank_genero([(Genero, Qtd)|Resto]) :-
    genero_string_atom(String, Genero), % Converte átomo para string para exibição
    format('~s | Ouvidas: ~d~n', [String, Qtd]),
    printa_rank_genero(Resto).

imprime_rank_global([]).
imprime_rank_global([Stats|Resto]) :-
    U = Stats.usuario,
    format('~s (~s)~n', [U.nome, U.email]),
    format('  - Scrobbles: ~d~n~n', [Stats.scrobbles]),
    imprime_rank_global(Resto).

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

