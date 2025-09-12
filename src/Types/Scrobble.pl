:- module(scrobble, [scrobble/3]).

:- use_module(musica).

scrobble(musica(Titulo, Artista, Album, Genero, Duracao), EmailUsuario, Momento) :-
    string(EmailUsuario),
    string(Momento),
    musica(Titulo, Artista, Album, Genero, Duracao).
