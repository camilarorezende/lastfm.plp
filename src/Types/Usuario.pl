:- module(usuario, [usuario/4]).

usuario(Nome, Email, Senha, Conquistas) :-
    string(Nome),
    string(Email),
    string(Senha),
    is_list(Conquistas),
    forall(member(C, Conquistas), string(C)).
