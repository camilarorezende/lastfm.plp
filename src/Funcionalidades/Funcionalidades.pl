:- dynamic usuario/4.
:- dynamic usuario/2.
:- dynamic musica/6.
:- dynamic scrobble/2.

:- use_module(library(http/json)).

% Salvar usuários em arquivo JSON
salvar_usuarios_json(File) :-
    findall(
        json([nome=Nome, email=Email, senha=Senha, conquistas=Conqs]),
        usuario(Nome, Email, Senha, Conqs),
        UsuariosJSON
    ),
    open(File, write, Stream),
    json_write(Stream, UsuariosJSON),
    close(Stream).

% Carregar usuários de arquivo JSON
carregar_usuarios_json(File) :-
    exists_file(File),
    open(File, read, Stream),
    json_read(Stream, UsuariosJSON),
    close(Stream),
    retractall(usuario(_,_,_,_)),
    carregar_lista_usuarios(UsuariosJSON).
carregar_usuarios_json(_).  % Caso o arquivo não exista, não faz nada

carregar_lista_usuarios([]).
carregar_lista_usuarios([json([nome=Nome, email=Email, senha=Senha, conquistas=Conqs]) | T]) :-
    assertz(usuario(Nome, Email, Senha, Conqs)),
    carregar_lista_usuarios(T).

% Mostrar conquistas de um usuário
ver_conquistas(Email) :-
    usuario(_, Email, _, Conqs),
    ( Conqs = [] ->
        write('Você ainda não possui conquistas.'), nl
    ;
        write('Suas conquistas:'), nl,
        forall(member(C, Conqs), (write('- '), write(C), nl))
    ), !.
ver_conquistas(_) :-
    write('Usuário não encontrado.'), nl.

% Adicionar conquista a um usuário (e salvar JSON)
adicionar_conquista(Email, Conquista) :-
    usuario(Nome, Email, Senha, Conqs),
    ( member(Conquista, Conqs) ->
        write('Usuário já possui essa conquista.'), nl
    ;
        retract(usuario(Nome, Email, Senha, Conqs)),
        assertz(usuario(Nome, Email, Senha, [Conquista | Conqs])),
        salvar_usuarios_json('usuarios.json'),  % salva automaticamente
        write('Conquista adicionada com sucesso!'), nl
    ), !.
adicionar_conquista(_, _) :-
    write('Usuário não encontrado.'), nl.

% Adicionar novo usuário (e salvar JSON)
adicionar_usuario(Nome, Email, Senha) :-
    \+ usuario(_, Email, _, _),
    assertz(usuario(Nome, Email, Senha, [])),
    salvar_usuarios_json('usuarios.json'),  % salva automaticamente
    write('Usuario cadastrado com sucesso!'), nl.
adicionar_usuario(_, Email, _) :-
    usuario(_, Email, _, _),
    write('Já existe um usuario com esse email!'), nl.

gerar_ranking_pessoal(EmailUsuario) :-
  findall(MusicaId, scrobble(EmailUsuario, MusicaId), Ids),
  ( Ids == [] ->
      writeln('Você ainda não tem nenhum scrobble :( Que tal dar play em alguma música? ;)')
  ; maplist(id_para_musica, Ids, Musicas),
    contar_ordenar_ids(Ids, ParesIdCont),
    maplist(par_id_para_par_musica, ParesIdCont, RankMusicas),
    writeln(''),
    writeln('Ranking das suas músicas mais escutadas! Veja seus hits do momento: '),
    imprime_rank_musicas(RankMusicas),
    maplist(musica_para_genero, Musicas, Generos),
    contar_ordenar_itens(Generos, RankGeneros),
    writeln(''),
    writeln('Ranking dos gêneros mais ouvidos:'),
    imprime_rank_generos(RankGeneros)
  ).

gerar_ranking_global :-
  findall((Email, Nome), usuario(Email, Nome), Usuarios),
  ( Usuarios == [] ->
      writeln(''),
      writeln('Nenhum usuário cadastrado ainda. Que tal ser o primeiro a se juntar ao LastFM? :)')
  ; construir_metricas(Usuarios, Metricas),
    maplist(metricas_chave, Metricas, Chaves),
    keysort(Chaves, Asc),
    reverse(Asc, Desc),
    maplist(segundo, Desc, Ordenadas),
    writeln(''),
    writeln('Ranking global do LASTFM baseado nos seus scrobbles! Os maiores ouvintes da nossa plataforma:'),
    writeln(''),
    imprime_ranking_global(Ordenadas)
  ).

id_para_musica(Id, musica(Id, Titulo, Artista, Album, Genero, Duracao)) :-
  musica(Id, Titulo, Artista, Album, Genero, Duracao).

musica_para_genero(musica(_, _, _, _, Genero, _), Genero).
musica_para_artista(musica(_, _, Artista, _, _, _), Artista).
musica_para_titulo(musica(_, Titulo, _, _, _, _), Titulo).
musica_duracao(musica(_, _, _, _, _, D), D).

contar_ordenar_itens(Itens, Ordenado) :-
  msort(Itens, Ord),
  clumped(Ord, Pares),
  maplist(inverter_par, Pares, Invertidos),
  keysort(Invertidos, Asc),
  reverse(Asc, Desc),
  maplist(inverter_par, Desc, Ordenado).

contar_ordenar_ids(Ids, Ordenado) :-
  contar_ordenar_itens(Ids, Ordenado).

par_id_para_par_musica((Id, C), (Musica, C)) :-
  id_para_musica(Id, Musica).

inverter_par(A-B, B-A).

imprime_rank_musicas([]).
imprime_rank_musicas([(musica(_, Titulo, Artista, _, _, _), Q)|R]) :-
  format('~w - ~w | Ouvidas: ~w~n', [Titulo, Artista, Q]),
  imprime_rank_musicas(R).

imprime_rank_generos([]).
imprime_rank_generos([(Genero, Q)|R]) :-
  format('~w | Ouvidas: ~w~n', [Genero, Q]),
  imprime_rank_generos(R).

construir_metricas([], []).
construir_metricas([(Email, Nome)|R], [metrica(Nome, Email, Qtd, Tempo, TopA, TopM)|Out]) :-
  findall(Id, scrobble(Email, Id), Ids),
  length(Ids, Qtd),
  maplist(id_para_musica, Ids, Musicas),
  maplist(musica_duracao, Musicas, Ds),
  sum_list(Ds, Tempo),
  maplist(musica_para_artista, Musicas, Arts),
  contar_ordenar_itens(Arts, ArtsOrd),
  take_n(3, ArtsOrd, TopA),
  maplist(musica_para_titulo, Musicas, Tits),
  contar_ordenar_itens(Tits, TitsOrd),
  take_n(3, TitsOrd, TopM),
  construir_metricas(R, Out).

metricas_chave(metrica(N, E, Q, T, A, M), Q-metrica(N, E, Q, T, A, M)).

segundo(_-V, V).

take_n(_, [], []).
take_n(0, _, []) :- !.
take_n(N, [X|Xs], [X|Ys]) :- N>0, N1 is N-1, take_n(N1, Xs, Ys).

imprime_ranking_global([]).
imprime_ranking_global([metrica(Nome, Email, Qtd, Tempo, TopA, TopM)|R]) :-
  format('~w (~w)~n', [Nome, Email]),
  format('  - Scrobbles: ~w~n', [Qtd]),
  string_tempo(Tempo, S),
  format('  - Tempo total escutado: ~w~n', [S]),
  writeln('  - Top artistas:'),
  imprime_nome_contagem(TopA),
  writeln('  - Top músicas:'),
  imprime_nome_contagem(TopM),
  writeln(''),
  imprime_ranking_global(R).

imprime_nome_contagem([]).
imprime_nome_contagem([(NomeItem, C)|R]) :-
  format('      * ~w (~w scrobbles)~n', [NomeItem, C]),
  imprime_nome_contagem(R).

string_tempo(Seg, S) :-
  H is Seg // 3600,
  R1 is Seg mod 3600,
  M is R1 // 60,
  Sg is R1 mod 60,
  format(string(S), '~0f~w ~0f~w ~0f~w', [H, 'h ', M, 'm ', Sg, 's']).