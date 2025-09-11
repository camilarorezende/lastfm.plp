% === Lista de conquistas fixas disponíveis ===
conquistas_disponiveis([
    "Primeiro Scrobble!",
    "10 músicas ouvidas",
    "100 minutos escutados"
]).
conquista(Usuario, 'Primeiro Scrobble') :-
    findall(_, scrobble(Usuario, _, _, _, _, _, _), Scs),
    length(Scs, N),
    N >= 1.
    
conquista(Usuario, "10 músicas ouvidas") :-
    findall(_, scrobble(Usuario, _, _, _, _, _, _), Scs),
    length(Scs, N),
    N >= 10.

conquista(Usuario, "100 minutos escutados") :-
    findall(Minutos, scrobble(Usuario, _, _, Minutos, _, _, _), Tempos),
    sum_list(Tempos, Total),
    Total >= 100.

conquista(Usuario, Conquista) :-
    findall(Artista, scrobble(Usuario, _, Artista, _, _, _, _), Artistas),
    most_common(Artistas, ArtistaMais),
    ArtistaMais \= none,
    string_concat('Super Fã de ', ArtistaMais, Conquista).

conquista(Usuario, Conquista) :-
    findall(Genero, scrobble(Usuario, _, _, _, Genero, _, _), Generos),
    most_common(Generos, GeneroMais),
    GeneroMais \= none,
    string_concat('Viciado em ', GeneroMais, Conquista).

% === Função auxiliar para encontrar o elemento mais frequente ===

most_common(Lista, ElementoMaisFrequente) :-
    setof(X, member(X, Lista), Unicos),
    contar_ocorrencias(Lista, Unicos, Contagens),
    sort(2, @>=, Contagens, Ordenado),
    Ordenado = [(ElementoMaisFrequente, _)|_].

contar_ocorrencias(_, [], []).
contar_ocorrencias(Lista, [X|Xs], [(X, N)|Contagens]) :-
    include(==(X), Lista, Ocorrencias),
    length(Ocorrencias, N),
    contar_ocorrencias(Lista, Xs, Contagens).

listar_conquistas(Usuario) :-
    setof(Conquista, conquista(Usuario, Conquista), Conquistas),
    format('Conquistas de ~w:\n', [Usuario]),
    forall(member(C, Conquistas), format('- ~w\n', [C])).