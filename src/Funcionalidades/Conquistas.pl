
:- module(conquistas, [
    get_conquistas_usuario/2 
]).

:- use_module(library(lists)).
:- use_module(library(solution_sequences)). 
get_conquistas_usuario(ScrobblesUsuario, TodasConquistas) :-
    length(ScrobblesUsuario, TotalScrobbles),
    findall(D, (member(S, ScrobblesUsuario), D = S.musica.duracao), Duracoes),
    sum_list(Duracoes, TotalSegundos),
    TotalMinutos is TotalSegundos // 60,
    regras(TotalScrobbles, TotalMinutos, ConquistasFixas),
    findall(A, (member(S, ScrobblesUsuario), A = S.musica.artista), Artistas),
    group_count(Artistas, ContagemArtistas),
    findall(Conquista, (
        member((Artista, N), ContagemArtistas),
        N >= 50,
        format(string(Conquista), 'Super Fã de ~s', [Artista])
    ), SuperFaArtistas),
    findall(musica(T, A), (
        member(S, ScrobblesUsuario),
        M = S.musica, T = M.titulo, A = M.artista
    ), MusicasChave),
    group_count(MusicasChave, ContagemMusicas),
    findall(Conquista, (
        member((musica(T, A), N), ContagemMusicas),
        N >= 50,
        format(string(Conquista), 'Super Fã da música ~s - ~s', [T, A])
    ), SuperFaMusicas),
    append(ConquistasFixas, SuperFaArtistas, TempConquistas),
    append(TempConquistas, SuperFaMusicas, TodasConquistas).
regras(TotalScrobbles, TotalMinutos, Conquistas) :-
    findall(Conquista,
            regra_valida(TotalScrobbles, TotalMinutos, Conquista),
            Conquistas).

regra_valida(TS, _, "Primeiro Scrobble!")     :- TS >= 1.
regra_valida(TS, _, "10 músicas ouvidas")      :- TS >= 10.
regra_valida(_, TM, "100 minutos escutados") :- TM >= 100.
group_count(Lista, Contagem) :-
    msort(Lista, ListaOrdenada),
    clumped(ListaOrdenada, Agrupado), 
    maplist(item_contagem, Agrupado, Contagem). 

item_contagem(Grupo, (Item, Contagem)) :-
    Grupo = [Item|_],
    length(Grupo, Contagem).

conquistas_disponiveis([
    "Primeiro Scrobble!",
    "10 músicas ouvidas",
    "100 minutos escutados"
  
]).
