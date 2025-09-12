:- use_module(library(lists)).
:- use_module(library(apply)).

:- dynamic scrobble/7.

conquistas_disponiveis([
  "Primeiro Scrobble!",
  "10 músicas ouvidas",
  "100 minutos escutados"
]).

conquista(Usuario, "Primeiro Scrobble!") :-
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
  mais_frequente_ou_none(Artistas, Mais),
  Mais \= none,
  string_concat("Super Fã de ", Mais, Conquista).

conquista(Usuario, Conquista) :-
  findall(Genero, scrobble(Usuario, _, _, _, Genero, _, _), Generos),
  mais_frequente_ou_none(Generos, Mais),
  Mais \= none,
  string_concat("Viciado em ", Mais, Conquista).

mais_frequente_ou_none([], none).
mais_frequente_ou_none(Lista, ItemMais) :-
  msort(Lista, Ordenada),
  clumped(Ordenada, Pares),
  maplist(inverter_par, Pares, Inver),
  keysort(Inver, Asc),
  reverse(Asc, [Freq-ItemMais|_]).

inverter_par(Item-Freq, Freq-Item).

listar_conquistas(Usuario) :-
  findall(C, conquista(Usuario, C), Todas),
  sort(Todas, Conquistas),
  format('Conquistas de ~w:\n', [Usuario]),
  ( Conquistas == [] ->
      true
  ; forall(member(C, Conquistas), format('- ~w\n', [C]))
  ).
