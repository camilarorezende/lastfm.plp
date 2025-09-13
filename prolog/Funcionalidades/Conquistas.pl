conquistas_disponiveis([
    "Primeiro Scrobble!",
    "10 músicas ouvidas",
    "100 minutos escutados"
]).

% === Regras para conquistas fixas ===
conquista(Scrobbles, UsuarioEmail, "Primeiro Scrobble!") :-
    include(scrobble_do_usuario(UsuarioEmail), Scrobbles, Scs),
    length(Scs, N),
    N >= 1.

conquista(Scrobbles, UsuarioEmail, "10 músicas ouvidas") :-
    include(scrobble_do_usuario(UsuarioEmail), Scrobbles, Scs),
    length(Scs, N),
    N >= 10.

conquista(Scrobbles, UsuarioEmail, "100 minutos escutados") :-
    include(scrobble_do_usuario(UsuarioEmail), Scrobbles, Scs),
    findall(D,
        (member(S, Scs), get_dict(musica, S, Musica), get_dict(duracao, Musica, D)),
        Duracoes),
    sum_list(Duracoes, Total),
    LimiteSegundos = 6000,  % 100 minutos
    Total >= LimiteSegundos.

% Super Fã de [Artista] — precisa de pelo menos 10 scrobbles do mesmo artista
conquista(Scrobbles, UsuarioEmail, Conquista) :-
    include(scrobble_do_usuario(UsuarioEmail), Scrobbles, Scs),
    findall(Artista,
        (member(S, Scs),
         get_dict(musica, S, M),
         get_dict(artista, M, Artista)),
        Artistas),
    contar_ocorrencias(Artistas, Contagens),
    member((ArtistaMais, N), Contagens),
    N >= 10,
    string_concat("Super Fã de ", ArtistaMais, Conquista).

% Viciado em [Gênero] — precisa de pelo menos 10 scrobbles do mesmo gênero
conquista(Scrobbles, UsuarioEmail, Conquista) :-
    include(scrobble_do_usuario(UsuarioEmail), Scrobbles, Scs),
    findall(Genero,
        (member(S, Scs),
         get_dict(musica, S, M),
         get_dict(genero, M, Genero)),
        Generos),
    contar_ocorrencias(Generos, Contagens),
    member((GeneroMais, N), Contagens),
    N >= 10,
    string_concat("Viciado em ", GeneroMais, Conquista).

most_common([], none).
most_common(Lista, ElementoMaisFrequente) :-
    setof(X, member(X, Lista), Unicos),
    contar_ocorrencias(Lista, Unicos, Contagens),
    sort(2, @>=, Contagens, Ordenado),
    Ordenado = [(ElementoMaisFrequente, _)|_].

contar_ocorrencias(Lista, Contagens) :-
    setof(X, member(X, Lista), Unicos),
    contar_ocorrencias(Lista, Unicos, Contagens).

contar_ocorrencias(_, [], []).
contar_ocorrencias(Lista, [X|Xs], [(X, N)|Contagens]) :-
    include(==(X), Lista, Ocorrencias),
    length(Ocorrencias, N),
    contar_ocorrencias(Lista, Xs, Contagens).

% Verifica se scrobble pertence ao usuário
scrobble_do_usuario(Email, Scrobble) :-
    get_dict(emailUsuario, Scrobble, EmailScrobble),
    string_lower(Email, E1),
    string_lower(EmailScrobble, E2),
    E1 == E2.

% Listar conquistas de um usuário (diagnóstico/debug)
listar_conquistas(UsuarioEmail) :-
    carregar_scrobbles(Scrobbles),
    findall(Conq, conquista(Scrobbles, UsuarioEmail, Conq), Conquistas),
    writeln('Conquistas desbloqueadas:'),
    forall(member(C, Conquistas), format('- ~w~n', [C])).
