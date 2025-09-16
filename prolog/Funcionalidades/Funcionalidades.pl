:- module(funcionalidades, [
    carregar_usuarios_json/0,
    carregar_usuarios/1,
    carregar_musicas/1,
    carregar_scrobbles/1,
    cadastrar_usuario/3,
    login/3,
    estatisticas_globais/0,
    gerar_ranking_global/0,
    gerar_ranking_pessoal/1,
    ver_conquistas/1,
    registrar_scrobble/2,
    recomendar_musicas/4,
    escolher_genero/1,
    listar_musicas/2,
    listar_scrobbles_usuario/1,
    usuario_para_dict/2,
    verificar_compatibilidade/2,
    verificar_compatibilidade/3
]).

:- dynamic usuario/4.
:- use_module(library(http/json)).
:- use_module(library(date)).
:- use_module(library(apply)).
:- use_module(library(random)).



:- ensure_loaded('../catalogo.pl').
:- ensure_loaded('Conquistas.pl').

% --------------------- Caminhos dos arquivos JSON ---------------------

caminho_usuarios('../usuarios.json').
caminho_scrobbles('../scrobbles.json').

% --------------------- Validações ---------------------

valid_usuario(Nome) :-
    string(Nome),
    re_match("^[a-zA-Z ]+$", Nome).

valid_email(Email) :-
    string(Email),
    re_match("^[\\w._%+-]+@[\\w.-]+\\.[a-zA-Z]{2,}$", Email).

% --------------------- Helpers de conversão/normalização ---------------------

valor_para_string(V, S) :-
    ( string(V) -> S = V
    ; atom(V)   -> atom_string(V, S)
    ; number(V) -> number_string(V, S)
    ; S = ""
    ).

padronizar_texto(Texto, Padrao) :-
    ( atom(Texto) -> atom_string(Texto, TextoStr) ; TextoStr = Texto ),
    split_string(TextoStr, "\n\r\t ", "\n\r\t ", Parts),
    atomics_to_string(Parts, " ", SemEspacos),
    string_lower(SemEspacos, Padrao).

titulo_normalizado(MusicaDict, TNorm) :-
    ( get_dict(titulo, MusicaDict, T0) -> valor_para_string(T0, TS) ; TS = "" ),
    padronizar_texto(TS, TNorm).

ja_ouviu(TitulosOuvidosNorm, MusicaDict) :-
    titulo_normalizado(MusicaDict, TNorm),
    member(TNorm, TitulosOuvidosNorm).

extrair_titulos_ouvidos(Historico, TitulosNormUnicos) :-
    findall(TNorm, (
        member(S, Historico),
        get_dict(musica, S, M),
        get_dict(titulo, M, T0),
        valor_para_string(T0, TS),
        padronizar_texto(TS, TNorm)
    ), Ts),
    list_to_set(Ts, TitulosNormUnicos).

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
    (   exists_file(File)
    ->  open(File, read, Stream),
        json_read_dict(Stream, UsuariosJSON),
        close(Stream),
        retractall(usuario(_, _, _, _)),
        carregar_lista_usuarios(UsuariosJSON)
    ;   true
    ).

carregar_lista_usuarios([]).
carregar_lista_usuarios([Elem|T]) :-
    (   Elem = json(Assoc) -> dict_create(Dict, _, Assoc)
    ;   Dict = Elem
    ),
    Nome  = Dict.nome,
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
    writeln('\nUsuário cadastrado com sucesso!').
cadastrar_usuario(Nome, Email, _) :-
    ( \+ valid_usuario(Nome) -> writeln('\nNome inválido: deve conter apenas letras e espaços')
    ; \+ valid_email(Email)   -> writeln('\nEmail inválido. Tente novamente.')
    ; usuario(_, Email, _, _) -> writeln('\nJá existe um usuário com esse email!')
    ).

login(EmailInput, SenhaInput, usuario{nome:Nome, email:Email}) :-
    usuario(Nome, Email, Senha, _),
    string_lower(Email, EmailLower),
    string_lower(EmailInput, EmailInputLower),
    EmailLower == EmailInputLower,
    SenhaInput == Senha.

% --------------------- Músicas (a partir dos fatos musica/5 do catálogo) ---------------------

carregar_musicas(MusicasDict) :-
    findall(
        musica{titulo:T, artista:A, album:Al, genero:G, duracao:D},
        musica(T, A, Al, G, D),
        MusicasDict
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
        catch( ( open(File, read, Stream),
                 json_read(Stream, JSON),
                 close(Stream),
                 converter_scrobbles(JSON, Scrobbles)
               ),
               _, Scrobbles = [] )
    ; Scrobbles = []
    ).

converter_scrobbles([], []).
converter_scrobbles([json(Obj)|T], [Novo|ST]) :-
    dict_create(Dict, _, Obj),
    (   get_dict(musica, Dict, M0)
    ->  ( M0 = json(MA) -> dict_create(M, _, MA) ; M = M0 ),
        put_dict(musica, Dict, M, Novo)
    ;   Novo = Dict
    ),
    converter_scrobbles(T, ST).

listar_scrobbles_usuario(Scrobbles) :- listar_scrobbles_usuario(Scrobbles, 1).
listar_scrobbles_usuario([], _).
listar_scrobbles_usuario([S|Ss], N) :-
    format('  ~d. ~w - ~w | ~w~n', [N, S.musica.titulo, S.musica.artista, S.momento]),
    N1 is N + 1,
    listar_scrobbles_usuario(Ss, N1).

registrar_scrobble(EmailUsuario, MusicaDict) :-
    get_time(TS),
    stamp_date_time(TS, DT, local),
    format_time(atom(Momento), '%Y-%m-%d %H:%M:%S', DT),
    Sc = scrobble{ emailUsuario:EmailUsuario, momento:Momento, musica:MusicaDict },
    salvar_scrobble(Sc),
    verificar_e_aplicar_conquistas(EmailUsuario).

salvar_scrobble(Novo) :-
    carregar_scrobbles(Scs),
    append(Scs, [Novo], Todos),
    salvar_scrobbles_json(Todos).

salvar_scrobbles_json(Scrobbles) :-
    caminho_scrobbles(File),
    maplist(scrobble_para_json, Scrobbles, JSONList),
    open(File, write, Stream),
    json_write_dict(Stream, JSONList),
    close(Stream).

scrobble_para_json(Sc, json([emailUsuario=Sc.emailUsuario, momento=Sc.momento, musica=Sc.musica])).

% --------------------- Conquistas ---------------------

ver_conquistas(Email) :-
    usuario(_, Email, _, Conqs),
    ( Conqs == [] -> writeln('\nVocê ainda não possui conquistas.')
    ; forall(member(C, Conqs), format('- ~w~n', [C]))
    ), !.
ver_conquistas(_) :- writeln('\nUsuário não encontrado.').

adicionar_conquista(Email, Conquista) :-
    usuario(Nome, Email, Senha, Conqs),
    ( member(Conquista, Conqs) ->
        writeln('\nUsuário já possui essa conquista.')
    ; retract(usuario(Nome, Email, Senha, Conqs)),
      assertz(usuario(Nome, Email, Senha, [Conquista|Conqs])),
      salvar_usuarios_json,
      writeln('\nNova conquista desbloqueada!')
    ), !.
adicionar_conquista(_, _) :- writeln('\nUsuário não encontrado.').

verificar_e_aplicar_conquistas(EmailUsuario) :-
    carregar_scrobbles(Scs),
    findall(C,
        ( conquista(Scs, EmailUsuario, C),
          \+ ja_tem_conquista(EmailUsuario, C)
        ),
        Novas),
    forall(member(X, Novas), adicionar_conquista(EmailUsuario, X)).

ja_tem_conquista(Email, C) :-
    usuario(_, Email, _, Conqs),
    member(C, Conqs).

% --------------------- Estatísticas ---------------------

tempo_total_usuario(Scs, usuario(_, Email, _, _), Total) :-
    include({Email}/[S]>>(get_dict(emailUsuario, S, E), atom_string(E, Email)), Scs, ScsU),
    findall(Dur, (member(S, ScsU), get_dict(musica, S, M), (get_dict(duracao, M, Dur) -> true ; Dur = 0)), Ds),
    sum_list(Ds, Total).

estatisticas_globais :-
    carregar_usuarios(Usuarios),
    carregar_scrobbles(Scs),
    ( Scs == [] ->
        writeln('\nNenhum dado disponível para estatísticas globais ainda.')
    ; findall(Art, (member(S, Scs), get_artista(S, Art)), Arts),
      max_por_frequencia(Arts, ArtTop, CArt),
      findall((T, A), (member(S, Scs), get_musica(S, T, A)), Mus),
      max_por_frequencia(Mus, (TT, AT), CMus),

      include({Scs}/[U]>>tem_scrobble(Scs, U), Usuarios, UsComSc),
      findall(TTt, (member(U, UsComSc), tempo_total_usuario(Scs, U, TTt)), Ts),
      length(Ts, Q),
      ( Q > 0 -> sum_list(Ts, Soma), TM is Soma / Q ; TM = 0 ),
      format_tempo(TM, F),

      format('\nEstatísticas Globais da Plataforma:\n'),
      format('\nArtista mais ouvido: ~w (~d scrobble(s))\n', [ArtTop, CArt]),
      format('Música mais ouvida: ~w - ~w (~d scrobble(s))\n', [TT, AT, CMus]),
      format('Tempo médio de escuta por usuário: ~w\n', [F])
    ).

gerar_ranking_pessoal(EmailU) :-
    carregar_scrobbles(Scs),
    findall(MD,
        ( member(S, Scs),
          get_dict(emailUsuario, S, E), string_lower(E, EL),
          string_lower(EmailU, UL), EL == UL,
          get_dict(musica, S, MD)
        ),
        MsDict),
    ( MsDict == [] ->
        writeln('\nVocê ainda não tem nenhum scrobble :( Que tal dar play em alguma música? ;)')
    ; maplist(dict_para_musica_termo, MsDict, Ms),
      contar_frequencias(Ms, CMs), ordenar_por_valor_decrescente(CMs, RankM),
      findall(G, (member(musica(_, _, G), Ms)), Gens),
      contar_frequencias(Gens, CGs), ordenar_por_valor_decrescente(CGs, RankG),
      writeln('\nRanking das suas músicas mais escutadas! Veja seus hits do momento:'),
      imprimir_rank_musicas(RankM),
      writeln('\nRanking dos gêneros mais ouvidos:'),
      imprimir_rank_generos(RankG)
    ).

dict_para_musica_termo(D, musica(TS, AS, GS)) :-
    valor_para_string(D.titulo, TS),
    valor_para_string(D.artista, AS),
    valor_para_string(D.genero,  GS).

contar_frequencias(Lista, Contagem) :-
    setof((X,N), aggregate(count, member(X, Lista), N), Contagem).

ordenar_por_valor_decrescente(Lista, Ordenada) :-
    predsort(compara_valor_decrescente, Lista, Ordenada).

compara_valor_decrescente(Delta, (X,V1), (Y,V2)) :-
    ( V1 > V2 -> Delta = '<'
    ; V1 < V2 -> Delta = '>'
    ; compara_empate(X, Y, Delta)
    ).

compara_empate(musica(T1,_,_), musica(T2,_,_), Delta) :- compare(Delta, T1, T2).
compara_empate(X, Y, Delta) :- compare(Delta, X, Y).

imprimir_rank_musicas([]).
imprimir_rank_musicas([(musica(T,A,_), Q)|R]) :-
    format('~w - ~w | Ouvidas: ~d~n', [T, A, Q]),
    imprimir_rank_musicas(R).

imprimir_rank_generos([]).
imprimir_rank_generos([(G, Q)|R]) :-
    format('~w | Ouvidas: ~d~n', [G, Q]),
    imprimir_rank_generos(R).

gerar_ranking_global :-
    carregar_usuarios(Usuarios),
    carregar_scrobbles(Scs),
    ( Usuarios == [] ->
        writeln('\nNenhum usuário cadastrado ainda. Que tal ser o primeiro a se juntar ao LastFM? :)')
    ; findall(Stat,
        ( member(usuario(N, E, _, _), Usuarios),
          include(scrobble_do_usuario(E), Scs, ScsU),
          length(ScsU, Qtd),
          Stat = stat{nome:N, email:E, scrobbles:Qtd}
        ),
        ListaStats),
      predsort(compare_scrobbles, ListaStats, Ranking),
      writeln('\n========= RANKING GLOBAL =========='), imprime_rank_global(Ranking)
    ).

scrobble_do_usuario(Email, Sc) :-
    get_dict(emailUsuario, Sc, E0),
    ( atom(E0) -> atom_string(E0, E) ; E = E0 ),
    ( atom(Email) -> atom_string(Email, ES) ; ES = Email ),
    string_lower(ES, L1), string_lower(E, L2), L1 == L2.

compare_scrobbles(<, A, B) :- A.scrobbles > B.scrobbles.
compare_scrobbles(>, A, B) :- A.scrobbles < B.scrobbles.
compare_scrobbles(=, _, _).

imprime_rank_global([], _).
imprime_rank_global([S|T], Pos) :-
    format('~d. ~w (~w) - ~d scrobble(s)~n', [Pos, S.nome, S.email, S.scrobbles]),
    Pos1 is Pos + 1,
    imprime_rank_global(T, Pos1).
imprime_rank_global(R) :- imprime_rank_global(R, 1).

verificar_compatibilidade(U1, U2) :-
    verificar_compatibilidade(U1, U2, C),
    P is C * 100,
    format('\nCompatibilidade entre ~w e ~w: ~2f%~n', [U1.nome, U2.nome, P]),
    ( P < 50  -> writeln('\nXiii... vocês têm gostos bem diferentes! Talvez devam encontrar algo em comum. :v')
    ; P >= 80  -> writeln('\nWow!!! Que match hein?! Perfeito para montarem uma playlist :D')
    ; writeln('\nNada mau!! Que tal fortalecerem esse laço?! ;)')
    ).

verificar_compatibilidade(U1, U2, Compatibilidade) :-
    carregar_scrobbles(Scs),
    include(scrobble_do_usuario(U1.email), Scs, S1),
    include(scrobble_do_usuario(U2.email), Scs, S2),
    findall(G, (member(S, S1), G = S.musica.genero), Gs1), list_to_set(Gs1, SG1),
    findall(A, (member(S, S1), A = S.musica.artista), As1), list_to_set(As1, SA1),
    findall(G, (member(S, S2), G = S.musica.genero), Gs2), list_to_set(Gs2, SG2),
    findall(A, (member(S, S2), A = S.musica.artista), As2), list_to_set(As2, SA2),
    intersection(SG1, SG2, ComG), length(ComG, LG),
    intersection(SA1, SA2, ComA), length(ComA, LA),
    ( length(SG1, LG1), LG1 > 0 -> CompG is LG / LG1 ; CompG = 0 ),
    ( length(SA1, LA1), LA1 > 0 -> CompA is LA / LA1 ; CompA = 0 ),
    Compatibilidade is (CompG * 0.6) + (CompA * 0.4).

% ---------- GÊNEROS MAIS OUVIDOS (para recomendação baseada no histórico) ----------

generos_mais_ouvidos(Historico, GenerosOrdenados) :-
    findall(GLower, (
        member(S, Historico),
        get_dict(musica, S, M),
        get_dict(genero, M, G0),
        valor_para_string(G0, GS),
        string_lower(GS, GLower)
    ), Generos),
    sort(Generos, Unicos),
    findall(Freq-Genero, (member(Genero, Unicos), count_occurrences(Genero, Generos, Freq)), Pares),
    sort(1, @>=, Pares, Ordenadas),
    pairs_values(Ordenadas, GenerosOrdenados).

count_occurrences(Elem, Lista, C) :- include(==(Elem), Lista, L), length(L, C).

sublist_aleatoria(List, N, Sub) :-
    randomize,
    random_permutation(List, Perm),
    take(N, Perm, Sub).

randomize :-
    set_random(seed(random)).
% ---------- Filtros de catálogo (gênero/artista com normalização) ----------

musica_tem_genero(GeneroDesejado, Musica) :-
    get_dict(genero, Musica, Gm),
    padronizar_texto(Gm, G1),
    padronizar_texto(GeneroDesejado, G2),
    G1 == G2.

musica_tem_artista(ArtistaDesejado, Musica) :-
    get_dict(artista, Musica, Am),
    padronizar_texto(Am, A1),
    padronizar_texto(ArtistaDesejado, A2),
    A1 == A2.

% ---------- Recomendação ----------


recomendar_musicas(Usuario, "1", Genero, MusicasRecomendadas) :-
    carregar_musicas(Todas),
    carregar_scrobbles(Scs),
    include(scrobble_do_usuario(Usuario.email), Scs, Hist),
    extrair_titulos_ouvidos(Hist, Titulos),
    include(musica_tem_genero(Genero), Todas, C0),
    exclude(ja_ouviu(Titulos), C0, Candidatas),
    sublist_aleatoria(Candidatas, 3, MusicasRecomendadas).


recomendar_musicas(Usuario, "2", Artista, MusicasRecomendadas) :-
    carregar_musicas(Todas),
    carregar_scrobbles(Scs),
    include(scrobble_do_usuario(Usuario.email), Scs, Hist),
    extrair_titulos_ouvidos(Hist, Titulos),
    include(musica_tem_artista(Artista), Todas, C0),
    exclude(ja_ouviu(Titulos), C0, Candidatas),
    sublist_aleatoria(Candidatas, 3, MusicasRecomendadas).


recomendar_musicas(Usuario, "3", _Parametro, MusicasRecomendadas) :-
    carregar_musicas(Todas),
    carregar_scrobbles(Scs),
    include(scrobble_do_usuario(Usuario.email), Scs, Hist),
    (   % Caso 0: sem histórico → aleatórias do catálogo
        Hist == [] ->
        sublist_aleatoria(Todas, 3, MusicasRecomendadas)
    ;   % Caso 1: com histórico → usa ordem de gêneros
        generos_mais_ouvidos(Hist, GensOrd),
        extrair_titulos_ouvidos(Hist, Titulos),

        top_k_por_generos(GensOrd, Titulos, Todas, 3, C0),
        (   C0 \= [] ->
            MusicasRecomendadas = C0
        ;   % Caso 2: ainda vazio → tenta completar só com o gênero #1
            (   GensOrd = [GTop|_] ->
                include(musica_tem_genero(GTop), Todas, CTop0),
                exclude(ja_ouviu(Titulos), CTop0, CTop),
                (   CTop \= [] ->
                    take(3, CTop, MusicasRecomendadas)
                ;   % Caso 3: não sobrou nada do top gênero → sorteia das NÃO ouvidas
                    exclude(ja_ouviu(Titulos), Todas, NaoOuvidas),
                    (   NaoOuvidas \= [] ->
                        sublist_aleatoria(NaoOuvidas, 3, MusicasRecomendadas)
                    ;   % Último fallback: sorteia do catálogo inteiro
                        sublist_aleatoria(Todas, 3, MusicasRecomendadas)
                    )
                )
            ;   % Sem gêneros ordenados (defensivo): cai para aleatórias
                sublist_aleatoria(Todas, 3, MusicasRecomendadas)
            )
        )
    ).

top_k_por_generos([], _Titulos, _Musicas, _K, []) :- !.
top_k_por_generos(_Gs, _Titulos, _Musicas, 0, []) :- !.
top_k_por_generos([G|Gs], Titulos, Musicas, K, Recs) :-
    % Candidatas deste gênero (a função musica_tem_genero/2 já normaliza internamente)
    include(musica_tem_genero(G), Musicas, C0),
    exclude(ja_ouviu(Titulos), C0, CandidatasG),

    take(K, CandidatasG, UsadasG),
    length(UsadasG, NUsadas),
    KRest is K - NUsadas,

    ( KRest =:= 0 ->
        Recs = UsadasG
    ;   top_k_por_generos(Gs, Titulos, Musicas, KRest, RecsRest),
        append(UsadasG, RecsRest, Recs)
    ).

eh_musica_mais_ouvida_genero(TitulosOuvidos, GenerosPreferidos, Musica) :-
    get_dict(titulo, Musica, T0),
    valor_para_string(T0, TS), string_lower(TS, TL),
    \+ member(TL, TitulosOuvidos),
    get_dict(genero, Musica, G0),
    valor_para_string(G0, GS), string_lower(GS, GL),
    member(GL, GenerosPreferidos).

% ---------- Utilidades ----------

sublist(List, N, Sub) :- length(Sub, N), append(Sub, _, List).

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|R]) :- N > 0, N1 is N - 1, take(N1, T, R).

get_artista(S, A) :- A = S.musica.artista.
get_musica(S, T, A) :- T = S.musica.titulo, A = S.musica.artista.
get_duracao(S, D) :- D = S.musica.duracao.

tem_scrobble(Scs, usuario(_, Email, _, _)) :-
    atom_string(EAtom, Email),
    member(S, Scs),
    get_dict(emailUsuario, S, EAtom).

max_por_frequencia(Lista, ElemMax, CMax) :-
    freq_lista(Lista, Freqs),
    keysort(Freqs, Ord),
    reverse(Ord, [CMax-ElemMax|_]).

freq_lista(Lista, Freqs) :-
    sort(Lista, Unicos),
    findall(C-E,
        ( member(E, Unicos), include(=(E), Lista, Sub), length(Sub, C) ),
        Freqs).

format_tempo(SegundosFloat, Out) :-
    Segundos is round(SegundosFloat),
    H is Segundos // 3600,
    M is (Segundos mod 3600) // 60,
    S is Segundos mod 60,
    format(atom(Out), '~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [H, M, S]).

conta_e_ordena(Lista, ContagemOrdenada) :-
    msort(Lista, LO),
    clumped(LO, Pares),
    maplist(item_contagem, Pares, Contagem),
    predsort(compare_contagem, Contagem, ContagemOrdenada).

compare_contagem(>, (_,C1), (_,C2)) :- C1 > C2.
compare_contagem(<, (_,C1), (_,C2)) :- C1 < C2.
compare_contagem(=, _, _).

item_contagem(Lista, (Item, C)) :- Lista = [Item|_], length(Lista, C).

usuario_para_dict(usuario(N,E,S,Conqs), usuario{nome:N, email:E, senha:S, conquistas:Conqs}).

% --------------------- UI: escolha de gênero ---------------------

escolher_genero(GeneroEscolhido) :-
    carregar_musicas(Ms),
    findall(G, (member(M, Ms), get_dict(genero, M, G)), Gs),
    sort(Gs, Unicos),
    writeln('\nGêneros disponíveis:'),
    listar_generos_numerados(Unicos, 1),
    write('\nEscolha o número do gênero: '), flush_output,
    read_line_to_string(user_input, Entrada),
    ( catch(number_string(N, Entrada), _, fail),
      nth1(N, Unicos, GeneroEscolhido) ->
        true
    ; writeln('\nOpção inválida. tente novamente.'), escolher_genero(GeneroEscolhido)
    ).

listar_generos_numerados([], _).
listar_generos_numerados([G|Gs], N) :-
    format('~d - ~w~n', [N, G]),
    N1 is N + 1,
    listar_generos_numerados(Gs, N1).
