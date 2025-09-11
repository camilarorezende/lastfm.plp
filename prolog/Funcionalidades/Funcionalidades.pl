:- dynamic usuario/4.
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
