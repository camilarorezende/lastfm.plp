:- module(musica, [musica/5]).

:- use_module(genero).

musica(Titulo, Artista, Album, Genero, Duracao) :-
    string(Titulo),
    string(Artista),
    string(Album),
    genero(Genero),
    integer(Duracao),
    Duracao >= 0.
