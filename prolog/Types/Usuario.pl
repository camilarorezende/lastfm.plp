
:- module(usuario, [
    is_usuario/1,          
    json_to_usuario/2,     
    usuario_to_json/2      
]).


is_usuario(Dict) :-
    is_dict(Dict, usuario).


json_to_usuario(JsonDict, UsuarioDict) :-
    UsuarioDict = JsonDict.put(tag, usuario).


usuario_to_json(UsuarioDict, JsonDict) :-
    del_dict(tag, UsuarioDict, _, JsonDict).